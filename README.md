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

A normalização de polinómios passa por analisar os monómios que o constituiem. Um polinómio normalizado deve ter os seus monómios ordenados por grau e variáveis que possuem.
Para tal, recorre-se a funções específicas de sorting que recorrem a `sortBy` pelas ordens e critérios que desejamos.
Deve também assegurar que não temos monómios com coeficiente nulos (estes são removidos). É igualmente necessário ter em atenção os expoentes nulos que levam à remoção da variável em questão.

### Somar Polinómios

Começando pelo caso mais simples, a soma de dois monómios é apenas possível caso estes possuem iguais variáveis com os mesmos expoentes. O resultado desta operação resulta em somar os coeficientes e manter as variáveis e respetivos expoentes.

A soma de dois polinómios nada mais é do que a concatenação de dois polinómios `([Monómio] ++ [Monómio])` seguida de uma tentativa de juntar todos os monómios que obedeçam ao critério de soma de monómios acima mencionado.

Para facilitar o processo, após ter uma única lista de monómios, normalizamo-la (operação explicada anteriormente) para garantir que monómios possíveis de serem somados estarão adjacentemente. Depois trata-se apenas de fazer chamadas recursivas que comparam cada par adjacente de monómios e, se compatíveis, os substitui pelo resultado da sua soma.

### Multiplicar Polinómios

Partindo do caso mais simples, isto é, a multiplicação de dois monómios, podemos depois extrapolar para o caso de polinómios.
A multiplicação envolve uma multiplicação simples dos coeficientes seguida de uma verificação das variáveis e respetivos expoentes.
Para tal, fazemos uma string de variáveis que seja o set das strings de cada um dos monómios e depois vamos a cada monómio ver que expoente corresponde a cada uma destas letras para somarmos os expoentes no resultado final.

Para multiplicar polinómios, basta aplicar uma operação distributiva a cada combinação de monómios que estes polinómios possuam.

### Derivar Polinómios

A derivada de polinómios é bastante simples. Trata-se apenas de aplicar uma função de derivar monómio a cada elemento usando um `map`.
Para derivar um monómio temos duas situações possíveis. Caso a variável em ordem à qual estamos a derivar não esteja presente na string de variáveis, o resultado é automaticamente zero.
Caso contrário, basta procurar o expoente dessa variável, subtrair-lhe 1 e multiplicar o antigo expoente pelo coeficiente do monómio.

### Parsing Output

O parsing da representação interna para string envolve, à semelhança de todas as operações anteriores, uma conversão de cada um dos monómios para string seguida de uma função para juntar cada uma dessas strings, obtendo, assim, o polinómio a ser printado.

Na passagem da representação interna escolhida para string, temos de ter em atenção várias coisas:

- o coeficiente pode ser nulo (ignorar termo) ou 1 (omitir coeficiente)
- o expoente pode ser 1 (omitir expoente)
- podemos estar perante um monómio simples, multivarável ou até mesmo perante uma constante

Para um output mais correto e organizado, é necessária a aplicação prévia de uma normalização ao polinómio a ser convertido para string.

## Exemplos de utilização

TODO

## Grupo

- Marco André (up202004891)
- Ricardo Matos (up202007962)

Finised on *22/10/2022*
