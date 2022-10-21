# PFL_T1

Este trabalho tem como objetivo desenvovler um programa Haskell que permita a manipulação simbólica de polinómios. Mais concretamente, devem ser implementadas as operações de soma, multiplicação e de derivação.

Para uma utilização mais fluida do programa, a conversão de uma string de input para o formato de representação interno e a conversão deste de volta para uma string de output devem ser implementadas.

## Funcionalidades Implementadas

Todas as funcionalidades previstas foram implementadas.

- [x] Parsing String &rarr; Polinómio
- [x] Normalização de polinómios
- [x] Soma de polinómios
- [x] Multiplicação de polinómios
- [x] Derivação de polinómios
- [x] Parsing Polinómio &rarr; String

## Configuração / Instalação / Funcionamento

O código desenvolvido foi testando tanto em Linux (Ubuntu) como em Windows 10, tendo como ficheiro principal **Proj.hs**. O programa pode ser compilado e executado usando:

```
ghc *.hs
```

ou através do interpretador GHCI com:

```
ghci Proj.hs
main
```

Foi também criado um simples Makefile para facilitar a vida com as seguintes opções:

- `make` - compila o código e limpa os ficheiros temporários no final
- `make clean` - remove ficheiros temporários no final
- `make run` - correr ficheiro executável
- `make check` - correr testes ao código (mais sobre testagem abaixo)

**Nota:** O Makefile possui comando Linux pelo que poderão não funcionar em Windows.

## Software Extra

Relativamente ao software, para além de todas as funcionalidades do Prelúdio, utilizamos também uma biblioteca para "Property Testing" do código chamada **QuickCheck**. Este pacote requer a instalação seguinte:

```
cabal install QuickCheck
```

**Nota :** Admite-se que o *cabal* já se encontre instalado na máquina.

### Troubleshooting

Durante o desenvolvimento deste trabalho verificaram-se vários problemas ao instalar e utilizar o *QuickCheck* tanto em Windows como em Linux pelo que é possível que o código não corra. Caso tal se verifique, recomenda-se os seguintes passos:

```Haskell
import Prop_tests -- Comentar o import dos módulo de testes (módulo em Prop_tests.hs) presente em Proj.hs

-- Comentar igualmente as linhas em Proj.hs que chamam a testagem do código 
main_test :: IO Bool
main_test = check
```

Por fim, pode ser removido o ficheiro "Prop_tests.hs" no qual se encontram os testes para evitar erros ao tentar fazer import do QuickCheck.

## Representação Interna

Para representar polinómios e monómios, foram criados os seguintes tipos:

```Haskell
-- Types definition
type Monomio = ((Int, [Int]), String)
type Polinomio = [Monomio]
```

A sua representação é bastante intuitiva. Um polinómio nada mais é do que um conjunto de monómios que por sua vez é representado por algo deste género:

```Haskell
3x^2y = ((3,[2,1]), "xy") -- 3  x^2   y^1
```

Um monómio é constituído por um par entre um par de (Int,[Int]) com String. Cada char da string indica uma variável do monómio, estando o expoente de cada uma destas variáveis guardado em [Int] pela mesma ordem. Por fim, o primeiro Int representa o coeficiente.
Esta abordagem é simples e como é uma composição de pares, permite tomar partido das funções `fst` e `snd` para aceder rapidamente a cada elemento.

## Estratégias de Implementação de Funcionalidades

### Parsing Input

TODO

### Normalizar Polinómio

TODO

### Somar Polinómios

TODO

### Multiplicar Polinómios

TODO

### Derivar Polinómios

TODO

### Parsing Output

TODO

## Exemplos de utilização

TODO

## Grupo

- Marco André (up202004891)
- Ricardo Matos (up202007962)

*22/10/2022*
