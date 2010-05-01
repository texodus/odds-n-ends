Copyright (c) 2009 Andrew Stein

Permission is hereby granted, free  of charge, to any person obtaining
a  copy  of this  software  and  associated  documentation files  (the
"Software"), to  deal in  the Software without  restriction, including
without limitation  the rights to  use, copy, modify,  merge, publish,
distribute,  sublicense, and/or sell  copies of  the Software,  and to
permit persons to whom the Software  is furnished to do so, subject to
the following conditions:

The  above  copyright  notice  and  this permission  notice  shall  be
included in all copies or substantial portions of the Software.

THE  SOFTWARE IS  PROVIDED  "AS  IS", WITHOUT  WARRANTY  OF ANY  KIND,
EXPRESS OR  IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE  WARRANTIES OF
MERCHANTABILITY,    FITNESS    FOR    A   PARTICULAR    PURPOSE    AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.






DATABASE.REDIS  -  an exercise in over engineering
______________________________________________________________________

Database.Redis is  a Haskell library for  interfacing with, obviously,
Redis  (http://code.google.com/p/redis),   a  fast  key-value  storage
server.  With it, you can do cool stuff like this:


  globalUserId = "global:nextUserId" 

  username = ("uid:"++) . (++":username") . show
  password = ("uid:"++) . (++":password") . show

  createUser name pass = run db $ do next <- incr globalUserId
                                     set (username next) user   
                                     set (password next) pass
                                     return next


It's also written in the "literate" style, which means you can read it
like  a comic  book  (with code  instead  of plot,  art, dialogue  and
creativity).  Drastically unlike a comic book, you can also compile it
(ghc --make redis.lhs).  

So anyway: here we are, empty  source file.  Gonna need a name of some
kind, and probably some pragmas and libraries ...

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE OverlappingInstances #-}
>
> module Database.Redis (Redis, run, query) where
>
> import qualified Data.Set as S
> import Data.List
> import Network
> import System.IO
> import Text.ParserCombinators.Parsec 
> import Control.Applicative hiding ((<|>))

Golden.






MODEL  -  what a Redis looks like in Haskell
______________________________________________________________________

- Monads, Functors, Applicatives ... are all ways of describing a data's abstract shape

> newtype Redis a = Redis  { unRedis :: Handle -> IO a }

- Now we need to describe Redis's datatypes in Haskell so conversions will be magic (tm)

> class Key a where
>
>     enkey :: a -> String
>     dekey :: String -> a
>
> class Value a where
>
>     parser :: Parser a
>     enval  :: a      -> String
>     deval  :: String -> a
>
>     deval     text   =  case parse parser "Response" text of
>                           (Right x) -> x

> data Void      = Ok | Error String
>                  deriving (Show)
>
> data RedisType = RString
>                  deriving (Show)

All we  need to connect to  a Redis server  is an IP, port  number and
(potentially)  a  password  but  one  of the  great  features  of  the
key-value  store  is  it's  remarkable  flexibility  in  the  form  of
sharding.  Since Redis has no concept of foreign keys nor any internal
consistency protection for adhoc schemas, partitioning key/value pairs
by some  discriminator has trivial CRUD  overhead - as long  as we map
keys to Redis instances via  consistent hashing, which is analogous to
mapping a well  distributed hash to the circumference  of a circle and
designating an  instance to each (n  / 2pi) arc.  Thus,  we'll need to
store multiple  server configurations, and we'll  need a discriminator
for each - the datatype I'll use for this configuration reflects this.

> type Password = String
>
> type Hash     = Int
>
> type Server   = [(String, PortNumber, Password, Hash)]






PARSING  -  revenge of the ghost pirate's ghost
______________________________________________________________________

Convert Redis' string data (API link here) to Haskell datatypes

> instance Key String where
>     enkey =  id
>     dekey =  id
>
> instance Value String where
>     enval  = show
>     parser = anyChar `manyTill` string "\r\n" 
>
> instance Value Void where
>     enval  = show
>     parser = (char '-' >> parser >>= (return . Error)) <|> (char '+' >> return Ok) 
>
> instance Value Int where
>     enval  = show
>     parser = do char ':' <|> return ':'
>                 x <- digit `manyTill` string "\r\n" 
>		  return $ read x 
>
> instance Value [String] where 
>     enval  = show
>     parser = do char '$'
>                 x <- parser 
>                 y <- count x anyChar
>                 return $ words y
>
> instance (Ord a, Value a, Show a) => Value (S.Set a) where
> --  enval  = undefined   TODO commented so it will throw a compiler warning
>     parser = do char '*'
>                 x <- parser
>                 y <- count (read x) $ char '$' >> (parser :: Parser String) >> parser
>                 return $ S.fromList y
>                       
> instance Value Bool where
>     enval  = show
>     parser = do char ':'
>                 x <- parser
>                 return $ case x of
>                            "1" -> True
>                            "0" -> False
>
> instance Value RedisType where
>     enval  = show
>     parser = do char '+'
>                 x <- parser
>                 return $ case x of
>                            "string" -> RString
>
> instance Value () where
>     enval  = \ _ -> ""
>     parser = return ()






CONTAINER  -  in which the author "totally wails on this one dude"
______________________________________________________________________

> db = [("127.0.0.1", 6379)]
>
> run :: Server     -> Redis a -> IO a
> run    ((ip,port):_) redis   =  do handle <- connectTo ip (PortNumber port)
>                                    r      <- hRun handle redis
>                                    hFlush handle >> (r `seq` hClose handle)
>                                    return r
>
> hRun :: Handle -> Redis a    -> IO a
> hRun    handle    (Redis q)  =  do hSetBuffering handle NoBuffering
>                                    q handle
>
> query :: Value a =>
>          String -> Redis a
> query    exec   =  Redis $ runQ 
> 
>     where runQ handle = do hPutStr handle exec
>                            hPutStr handle "\r\n"			
>                            x <- hGetContents handle
>                            return $ deval x
>
> bulk :: String -> String
> bulk    text   =  (show $ length text) ++ "\r\n" ++ text
>
> instance Monad Redis where
>     return x = Redis $ \ ______ -> return x
>     r >>= f  = Redis $ \ handle -> ((unRedis r) handle >>= (($ handle) . unRedis . f))
>
> instance Functor Redis where
>     fmap f r = Redis $ \ handle -> ((unRedis r) handle >>= (return . f))
>
> instance Applicative Redis where
>     pure x   = Redis $ \ ______ -> return x
>     x <*> y  = Redis $ \ handle -> ((unRedis x) handle <*> (unRedis y) handle) 




 

DSL  -  acronyms are for pussies
______________________________________________________________________

Connection handling

> quit ::           Redis ()
> auth :: String -> Redis Void
>
> quit      = query   "QUIT"
> auth pass = query $ "AUTH " ++ pass

Commands operating on all the kind of values

> exists    :: Key a          => a        -> Redis Bool
> del       :: Key a          => a        -> Redis Bool
> type'     :: Key a          => a        -> Redis RedisType
> keys      :: Key a          => a        -> Redis [String]
> randomkey ::                               Redis String
> rename    :: (Key a, Key b) => a -> b   -> Redis Void
> renamenx  :: (Key a, Key b) => a -> b   -> Redis Void
> dbsize    ::                               Redis Int
> expire    :: Key a          => a -> Int -> Redis Bool
> ttl       :: Key a          => a        -> Redis Int
> select    ::                   Int      -> Redis Void
> move      :: Key a          => a -> Int -> Redis Void
> flushdb   ::                               Redis Void
> flushall  ::                               Redis Void
>
> exists    key           = query $ "EXISTS "   ++ (enkey key)
> del       key           = query $ "DEL "      ++ (enkey key)
> type'     key           = query $ "TYPE "     ++ (enkey key)
> keys      patt          = query $ "KEYS "     ++ (enkey patt)
> randomkey               = query   "RANDOMKEY"
> rename    oldkey newkey = query $ "RENAME "   ++ (enkey oldkey) ++ " " ++ (enkey newkey)
> renamenx  oldkey newkey = query $ "RENAMENX " ++ (enkey oldkey) ++ " " ++ (enkey newkey)
> dbsize                  = query   "DBSIZE"
> expire    key    ttl    = query $ "EXPIRE "   ++ (enkey key) ++ " " ++ (show ttl)
> ttl       key           = query $ "TTL "      ++ (enkey key)
> select    dbid          = query $ "SELECT "   ++ (show dbid)
> move      key    dbid   = query $ "MOVE "     ++ (enkey key) ++ " " ++ (show dbid)
> flushdb                 = query   "FLUSHDB"
> flushall                = query   "FLUSHALL"

Commands operating on string values

> set    :: (Key a, Value b) => a -> b     -> Redis Void
> get    :: (Key a, Value b) => a          -> Redis b
> getset :: (Key a, Value b) => a -> b     -> Redis b
> mget   :: (Key a, Value b) => [a]        -> Redis b
> setnx  :: (Key a, Value b) => a -> b     -> Redis Bool
> mset   :: (Key a, Value b) => [a] -> [b] -> Redis Void
> msetnx :: (Key a, Value b) => [a] -> [b] -> Redis Void
> incr   :: Key a            => a          -> Redis Int
> incrby :: Key a            => a -> Int   -> Redis Int
> decr   :: Key a            => a          -> Redis Int
> decrby :: Key a            => a -> Int   -> Redis Int
>
> set    key  val  = query $ "SET "    ++ (enkey key) ++ " " ++ (bulk . enval) val
> get    key       = query $ "GET "    ++ (enkey key)
> getset key  val  = query $ "GETSET " ++ (enkey key) ++ " " ++ (bulk . enval) val
> mget   keys      = query $ "MGET "   ++ concat (intersperse " " (map enkey keys))
> setnx  key  val  = query $ "SETNX "  ++ (enkey key) ++ " " ++ (bulk . enval) val
> mset   keys vals = query $ "MSET "   ++ concat (intersperse " " (zipWith ((++) . (++ " ")) (map enkey keys) (map (bulk . enval) vals)))
> msetnx keys vals = query $ "MSETNX " ++ concat (intersperse " " (zipWith ((++) . (++ " ")) (map enkey keys) (map (bulk . enval) vals)))
> incr   key       = query $ "INCR "   ++ (enkey key)
> incrby key  x    = query $ "INCRBY " ++ (enkey key) ++ " " ++ (show x)
> decr   key       = query $ "DECR "   ++ (enkey key)
> decrby key  x    = query $ "DECRBY " ++ (enkey key) ++ " " ++ (show x)
 
Commands operating on lists

 rpush :: (Key a, Value b) => a -> b -> Redis Void
 lpush :: (Key a, Value b) => a -> b -> Redis Void


    * RPUSH key value Append an element to the tail of the List value at key
    * LPUSH key value Append an element to the head of the List value at key
    * LLEN key Return the length of the List value at key
    * LRANGE key start end Return a range of elements from the List at key
    * LTRIM key start end Trim the list at key to the specified range of elements
    * LINDEX key index Return the element at index position from the List at key
    * LSET key index value Set a new value as the element at index position of the List at key
    * LREM key count value Remove the first-N, last-N, or all the elements matching value from the List at key
    * LPOP key Return and remove (atomically) the first element of the List at key
    * RPOP key Return and remove (atomically) the last element of the List at key
    * RPOPLPUSH srckey dstkey Return and remove (atomically) the last element of the source List stored at _srckey_ and push the same element to the destination List stored at _dstkey_ 


