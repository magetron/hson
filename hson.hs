import Data.List

data JSONValue = JString String
			   | JNumber Double
	  		   | JBool Bool
	  		   | JArray [JSONValue]
	  		   | JObject [NamedJValue]

data NamedJValue = NamedJValue String JSONValue


