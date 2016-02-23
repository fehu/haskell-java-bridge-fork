{-# LANGUAGE Haskell2010 #-}


module Foreign.Java.Bindings.HaskellTypes where


data HaskellModule = HaskellModule {
    moduleName :: String,
    moduleFunctions :: [HaskellFunction],
    moduleData :: [HaskellData]
    
  } deriving (Show, Eq)


data HaskellData = HaskellData {
    dataName :: String,
    dataConstructors :: [HaskellConstructor]
    
    
  } deriving (Show, Eq)


data HaskellFunction = HaskellFunction {
    functionName :: String,
    functionArgs :: [HaskellType],
    functionReturn :: HaskellType
    
  } deriving (Show, Eq)


data HaskellConstructor = HaskellConstructor {
    constructorName :: String,
    constructorField :: (String, HaskellType)
    
  } deriving (Show, Eq)


data HaskellType = HaskellType {

  } deriving (Show, Eq)



