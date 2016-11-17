# Haskell, can we REST?
## JSON and Message Pack

One of the joys of truly RESTful services is that we can freely speak about resources and respresentations seperately. A robust REST service will observe the [Accepts](https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) header. The client sends the content type they want and the server responds with said content or a 406 error.

In Haskell we are lucky to have an eccosystem that often lowers the bar to exposing many content types. The [Aeson](https://hackage.haskell.org/package/aeson) library makes it a joy to both produce and consume [JSON](http://www.json.org/). It is quite fast, but JSON is not always appropriate.

### Service To Service Communication
Within a service based architecture services often communicate over REST. More often than not they use JSON. However, JSON is intended as a human readable transport. This make it wasteful in many ways. One alternative is [Message Pack](http://msgpack.org/).

Message Pack is not human readable, it is a transport medium intended for machine to machine communication. This could be a great option and Haskell has an evolving [Message Pack library](https://hackage.haskell.org/package/data-msgpack) 

### Supporting People and Computers
What if we have a service that needs to support people and computers? REST allows us to do this. What about the ecosystem? What is the cost of supporting two transport mediums? Can Haskell's Message Pack library actually perform as well or better than the venerable Aeson?

## Let's Code

### Some Boilerplate
First lets pull in some dependencies and setup some language extensions. We'll explain the important bits as we go.

~~~ {.haskell}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.DeepSeq (NFData)
import qualified Criterion.Main as Criterion
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.MessagePack as MsgPck
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Weigh as Weigh
import           Control.Exception (assert)
~~~

### What do we want to know?

What we want to know is: how does `Data.MessagePack` compare with `Data.Aeson`? We'll define this concretely:
* How fast is it to encode and decode?
* What is the memory footprint of encoding and decoding?
* Is its wire representation more compact?

At a high level we will:
* Test our benchmark
* Run timing benchmarks via [`Criterion`](https://hackage.haskell.org/package/criterion)
* Measure memory usage via [`Weigh`](https://hackage.haskell.org/package/weigh)
* Compare the byte length of the resulting payload.

~~~ {.haskell}
main :: IO ()
main = do
  test
  benchmark
  weigh
  payload
~~~

### Data
Here we define a record type that contains all the primitive types that Message Pack and Aeson support. We've also included a sum type to test Haskell types that are not natively encoded in these transports.

~~~ {.haskell}
data A
  = A
  { fieldB :: B
  , fieldText :: Text
  , fieldInt :: Int
  , fieldDouble :: Double
  , fieldFloat :: Float
  , fieldWord :: Word
  --, fieldBin :: ByteString -- Aeson lacks a ByteString instance
  , fieldMaybeText :: Maybe Text
  , fieldListText :: [Text]
  } deriving (Eq, Generic)

data B = B1 Int | B2 Float
  deriving (Eq, Generic)

testData = A
  (B1 1) "foo" 12 234.234123 123.34563 12 Nothing ["bar", "baz"]
~~~

### Encoding/Decoding
Previously we defined our data with `deriving (Generic)` utilizing the `DeriveGeneric` language extension. This will allow us to fall back to "generic" versions of functions if a library defines them.

`Data.Aeson` defines default generic implementations for its encoding and decoding. So we only need to declare an instance.

~~~ {.haskell}
instance Aeson.FromJSON A
instance Aeson.ToJSON A

instance Aeson.FromJSON B
instance Aeson.ToJSON B
~~~

Luckily `Data.MessagePack` does too.

~~~ {.haskell}
instance MsgPck.MessagePack A
instance MsgPck.MessagePack B
~~~

Wait, that was it? 6 lines of code? That was easy! We can now REST knowing that supporting multiple mediums is not burdensome on developers.

## Benchmarking

Now that we know we can easily support multiple transport mediums, does it make sense to do so?

### Testing

First we'll need to test that our encoding/decoding are working correctly. We'll define pickle functions that encode and then decode our data.

~~~ {.haskell}
pickleJson :: A -> Maybe A
pickleJson = Aeson.decode . Aeson.encode

pickleMsgPck :: A -> Maybe A
pickleMsgPck = MsgPck.unpack . MsgPck.pack
~~~

Now we can create a simple test function. This function asserts that our pickled data is the same as the original data. Since we automatically derived the `Eq` typeclass, we can do this by simply asserting equality.

~~~ {.haskell}
test = do
  assert (Just testData == pickleJson testData) $ pure ()
  assert (Just testData == pickleMsgPck testData) $ pure ()
~~~

### Need For Speed
We'll use our pickle function to check the speed of encoding/decoding, utilizing `Criterion`.

We can create a simple benchmark: 

~~~ {.haskell}
benchmark = do
  Criterion.defaultMain
    [ Criterion.bgroup "pickle"
      [ Criterion.bench "json" $ Criterion.nf pickleJson testData
      , Criterion.bench "msgpack" $ Criterion.nf pickleMsgPck testData
      ]
    ]
~~~

`defaultMain` will run groups of tests and display their results to the console, we only have one group. `bgroup` defines a group of tests and collects their results, again we only have one.

`bench` defines an actual benchmark. We are using the `nf` function. This function accepts a function to be run, and data to be applied to that function. Its type signature is:

```
nf :: NFData b => (a -> b) -> a -> Benchmarkable
```

Notice the `NFData` constraint. Haskell is a lazy language so evaluation of code happens on an as needed basis. `NFData` exposes the ability to fully evaluate a piece of data. This is important for our benchmarks because lazyness could skew results. We want Haskell to do all of the work to decode/encode when we ask it to.

`NFData` also has a generic implementation, so all we need to do is declare the instance.

~~~ {.haskell}
instance NFData A
instance NFData B
~~~

### Memory Consumption
Now lets talk memory. Message Pack wouldn't be worth much if it balloons the memory consumption of our server. We want to run on small boxes and we know Aeson can let us do that.

To test this we'll use `Weigh`. Weigh is a robust library, but we have simple needs. We are just testing a function, so we'll use `weightFunc`. This works similarly to `Criterion.bench`. It takes a function to be measured and data to be applied to it. It also has an `NFData` constraint, but we've happily already defined it.

~~~ {.haskell}
weighFunction :: (NFData a, NFData b)
              => String -> (a -> b) -> a -> IO ()
weighFunction label func dat = do

  (allocations, garbageCollected) <- Weigh.weighFunc func dat

  putStrLn $   "Weighing:              " ++ label
          ++ "\nAllocations Collected: " ++ show allocations
          ++ "\nGarbage Collected:     " ++ show garbageCollected
          ++ "\n"

weigh = do 
  weighFunction "aeson" pickleJson testData
  weighFunction "msgpack" pickleMsgPck testData
~~~

Weigh doesn't have standard reporting for `weighFunc`, so we've written a bit of IO to display our results.

### Payload Size
Message Pack is a binary format, so it should be more compact than JSON. We can prove this by simply encoding our test data and asking for the `length` of the resulting `ByteString`.

~~~ {.haskell}
payloadSize :: NFData a
            => String -> (a -> ByteString) -> a -> IO ()
payloadSize label func dat = do
  putStrLn $   "Payload Type:   " ++ label
          ++ "\nContent-Length: " ++ show (BS.length $ func dat)
          ++ "\n"

payload = do
  payloadSize "aeson" Aeson.encode testData
  payloadSize "msgpack" MsgPck.pack testData
~~~

## Results
Lets run it. This README.md is a literate haskell file. You can clone this repo and run this file with [stack](http://haskellstack.org).

```
$ stack build
$ stack exec json-msg-pack-bench

benchmarking pickle/json
time                 54.43 μs   (54.23 μs .. 54.75 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 54.54 μs   (54.35 μs .. 54.86 μs)
std dev              792.8 ns   (540.0 ns .. 1.115 μs)

benchmarking pickle/msgpack
time                 26.10 μs   (26.06 μs .. 26.16 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.16 μs   (26.11 μs .. 26.23 μs)
std dev              211.5 ns   (152.3 ns .. 316.2 ns)

Weighing:              aeson
Allocations Collected: 76904
Garbage Collected:     0

Weighing:              msgpack
Allocations Collected: 61608
Garbage Collected:     0

Payload Type:   aeson
Content-Length: 183

Payload Type:   msgpack
Content-Length: 37
```

## Conclusion
Message Pack is fast, it is compact, it is conservative and very appropriate for machine to machine communication. Haskell as well makes it perfectly enjoyable to support multiple transport layers with ease.
