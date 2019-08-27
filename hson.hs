import Data.List

data JSONValue = JString String
			   | JNumber Double
	  		   | JBool Bool
	  		   | JArray [ JSONValue ]
	  		   | JObject [ NamedJValue ]
	  		   | JNull 

data NamedJValue = NamedJValue String JSONValue

str_ :: String -> JSONValue
str_ = JString

num_ :: Double -> JSONValue
num_ = JNumber

true_ :: JSONValue
true_ = JBool True

false_ :: JSONValue
false_ = JBool False

null_ :: JSONValue
null_ = JNull

arr_ :: [ JSONValue ] -> JSONValue
arr_ = JArray

obj_ :: [ NamedJValue] -> JSONValue
obj_ = JObject

(.=) :: String -> JSONValue -> NamedJValue
name .= value = NamedJValue name value

infixr 9 .=

hson :: JSONValue -> String
hson jsval = case jsval of
			   JString str -> show str
			   JNumber n -> show n
			   JBool True -> "true"
			   JBool False -> "false"
			   JNull -> "null"
			   JArray arr -> "[ " ++ intercalate ", " (map hson arr) ++ " ]"
			   JObject elems -> "{ " ++ intercalate ", " (map hlement elems) ++ " }"
			   where 
				   hlement :: NamedJValue -> String
				   hlement (NamedJValue str jv') = show str ++ ": " ++ hson jv'

main :: IO()
main = do
	putStrLn (hson sample)

sample :: JSONValue
sample =
  obj_
    [ 	"language" .= str_ "Haskell",
		"age" .= num_ 19,
		"libraries" .= arr_
      		[ 	str_ "https://hackage.haskell.org/",
				str_ "https://stackage.org/"
      		]
    ]
