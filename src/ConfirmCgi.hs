import Ella.Framework

views = []

main :: IO ()
main = dispatchCGI views defaultDispatchOptions
