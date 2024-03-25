module Individuo
   ( Individuo,
   novoIndividuo,
   cromossomos,
   fitness,
   atualizarFitness
   ) where

data Individuo = MkIndividuo {cromossomos :: [Int], fitness :: Float} deriving (Show)

data Operacao = GetCromossomos | GetFitness | AtualizarFitness Float

limitarFitness :: Float -> Float
limitarFitness novoFitness = max 0.0 (min 1.0 novoFitness)

executarOp :: Individuo -> Operacao -> Individuo
executarOp (MkIndividuo c f) op = case op of
   GetCromossomos -> MkIndividuo c f
   GetFitness -> MkIndividuo c f
   AtualizarFitness novoFitness -> MkIndividuo c (limitarFitness novoFitness)

novoIndividuo :: [Int] -> Float -> Individuo
novoIndividuo cromossomos fitness = executarOp (MkIndividuo cromossomos fitness) GetCromossomos

atualizarFitness :: Individuo -> Float -> Individuo
atualizarFitness individuo novoFitness = executarOp individuo (AtualizarFitness novoFitness)

getCromossomos :: Individuo -> [Int]
getCromossomos individuo = case executarOp individuo GetCromossomos of
   MkIndividuo c _ -> c

getFitness :: Individuo -> Float
getFitness individuo = case executarOp individuo GetFitness of
   MkIndividuo _ f -> f

selecao :: [Individuo] -> Individuo
selecao [individuo] = individuo
selecao (individuo:demais) =
   let melhorDeTodos = selecao demais
   in if fitness individuo > fitness melhorDeTodos
      then individuo
      else melhorDeTodos