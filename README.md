# Homework 2: Temáticas do HW

## Descrição
Trabalho de...
## Estrutura

```text
homework-01/
├── codigo/
│   ├──                         # Conjunto de dados utilizado
│   ├── main.r                  # Código principal da análise
│   └── Rplots.pdf              # Gráficos gerados pelo R
│
├── relatorio/
│   ├── imagens/                # Imagens e gráficos utilizados no relatório
│   ├── main.tex                # Código-fonte do relatório LaTeX
│   └── main.pdf                # Relatório final renderizado
│
└── README.md                   # Documentação do projeto
```

## Divisão e Colaboração
O trabalho foi dividido em quatro tarefas para cada um dos integrantes e, como cada etapa era dependente da anterior, debatido em grupo para obter maior coerência entre as seções. A divisão foi:
1. Danny Secundino:
1. Saulo Gomes: 
1. Alexandre Grangeiro: 
1. Bruno Lage: 

De modo que todos redigiram o relatório. Quanto às discussões, chegamos a conclusões sobre a estrutura do relatório (divisão da questão em pontos), organização do cálculo manual (fórmulas, tabelas) e discrepância entre os métodos usados para cálculo do quartil (na questão 2, manual contra a função do R). Além disso, foram discutidas a escolha da variável usada a certa altura, a interpretação dos enunciados, a definição da variável low_usage e quais variáveis mais influenciavam no total_user a partir dos dados obtidos. 

## Como Rodar o Código
O código foi pensado para ter a sua saída apresentada em uma interface no terminal, então é de extrema importância que o código seja executado. Para isso, **com o R devidamente instalado** na sua máquina, navegue até a pasta `codigo/` e, no terminal digite:
```bash
Rscript main.r
```
