# Tanscheit_Santos_Ventura_Dados_OP_PT_2020

## Introdução 

Neste repositório, estão disponíveis os scripts (em R) e dados para replicação do artigo:

- [SANTOS, Fabiano; TANSCHEIT, Talita and VENTURA, Tiago. O Partido dos Trabalhadores e as Instituições Participativas: a Influência da Dinâmica Intrapartidária na Adoção do Orçamento Participativo. Dados. 2020, vol.63, n.3](https://www.scielo.br/scielo.php?script=sci_abstract&pid=S0011-52582020000300202&lng=en&nrm=iso&tlng=pt)


## Tutorial

O repósitório possui quatro arquivos principais. 

- `analises.r` = código em R para replicação do artigo. Para fins de replicação, considere ler as dependências dos pacotes na minha sessão de R. 

- `data.csv` = banco de dados do artigo. 

- `models.Rdata` = Cópia do meu R Workspace com todos os modelos e figuras salvas geradas a partie do código `analises.r`

- `descricao_banco_de_dados.md` = arquivo com a descrição das variáveis do banco de dados e sua fonte. 

Para baixar todos os arquivos, você pode digitar  em sua linha de comando: 

```bash
git clone https://github.com/TiagoVentura/Tanscheit_Santos_Ventura_Dados_OP_PT_202
```

Eu utilizo R Projects e o excelente pacote [`here`](https://www.tidyverse.org/blog/2017/12/workflow-vs-script/) para tornar minha dinâmica de trabalho em R completamente reproducível. Portanto, após fazer o download do diretório, você pode abrir o arquivo `Tanscheit_Santos_Ventura_Dados_OP_PT.Rproj` em seu `RStudio`, e o script de análise deve rodar sem problemas. 

