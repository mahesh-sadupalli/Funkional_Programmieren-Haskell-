data Division = Division Double Double deriving Show --Product type
data Grading = Passed String | Notpassed String | Revision String Grading deriving Show  --Sum Type 
data Weekday = Montag| Dienstag | Sonntag Weekday deriving Show --Enm type 
data Monat = Januar | Dezember Monat deriving Show --Enm type 
data Date = Date Integer Integer Integer deriving Show -- Product type 
data Time = Time Integer Integer Integer deriving Show --Product type 
data Price = Price Integer Integer deriving Show --Product 
data Cash = Cash Integer String deriving Show --product 
data Canteenmeal = Soup String | Meal1 String | Meal2 String |Vegan String deriving Show --sum type
data Box = Box Double Double deriving Show --Product
data Receipt = Receipt Cash Date Canteenmeal Price deriving Show --product type

