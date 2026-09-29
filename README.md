# SIM-RJ — Dados de Mortalidade por Município e Bairro

Script em **R** para automatizar a extração de dados do **Sistema de Informação sobre Mortalidade (SIM)** disponibilizados pelo **TabNet da Secretaria de Estado de Saúde do Rio de Janeiro (SES-RJ)**.

O código realiza consultas automatizadas para obter óbitos de residentes no estado do Rio de Janeiro, organizados por **município, bairro de residência, faixa etária, sexo e ano**, com foco nas doenças dos aparelhos **circulatório (CID-10 I00–I99)** e **respiratório (CID-10 J00–J99)**.

O período atualmente configurado é de **2006 a 2026**.

### Principais funcionalidades

* Extração automatizada dos dados do TabNet da SES-RJ;
* Consulta dos dados por ano, capítulo da CID-10 e sexo;
* Obtenção de informações de bairro de residência para os municípios do estado;
* Associação dos municípios aos respectivos códigos IBGE;
* Conversão dos resultados para formato longo;
* Sistema de **cache**, permitindo retomar a execução sem repetir consultas já realizadas;
* Exportação dos dados consolidados para CSV;
* Possibilidade de adaptação para outras variáveis e categorias disponíveis no TabNet.

### Observações sobre os dados

Os dados de bairro apresentam limitações importantes. Para o período de **2006 a 2010**, o bairro de residência não está disponível na base utilizada pelo sistema, sendo apresentado como não especificado/ignorado. A partir de 2011, parte dos registros permanece sem bairro informado e os nomes dos bairros podem apresentar diferentes formas de grafia.

Por esse motivo, os nomes dos bairros devem ser **padronizados antes de análises espaciais ou agregações por bairro**.

Os anos mais recentes podem conter **dados preliminares**, conforme a atualização da SES-RJ.

### Fonte

**Secretaria de Estado de Saúde do Rio de Janeiro — TabNet.**

Sistema de Informação sobre Mortalidade (SIM), formulário **Mortalidade Geral - RJ**.
