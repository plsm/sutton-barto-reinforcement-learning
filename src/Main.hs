import Util.RandomStreams

main = do
  mapM_ putStrLn (map show (take 20 (streamRandomPositiveInts 42)))
  mapM_ putStrLn (map show (take 20 (streamRandomUniformFloats 42)))
  mapM_ putStrLn (map show (take 20 (streamRandomNormalFloats 42)))
  mapM_ putStrLn (map show (take 20 (streamRandomFlips 42 0.5)))
