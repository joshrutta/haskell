module Test where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

-- 2

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3

getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer price) = manufacturer

-- 4 Will throw an Exception about Non-exhaustive patterns

-- 5

data Size = Size Integer deriving (Eq, Show)

data NewVehicle = NewCar Manufacturer Price | NewPlane Airline Size deriving (Eq, Show)
