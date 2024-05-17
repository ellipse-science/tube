# tube 🚰

![Le tube](tube.png)
(Image par [Dall-E 3](https://openai.com/dall-e-3))

Interface R d'accès à la plateforme de données _Ellipse_.

## Pré-requis

Pour accéder aux données de la plateforme, il faut configurer les clés d'accès AWS. Pour obtenir ces informations, contacter Patrick Poncet (@patoscope) sur Slack.

Ensuite, il faut les ajouter au fichier `~/.Renviron` qui est chargé au démarrage de la session R. Ce fichier réside à la racine de votre répertoire d'utilisateur. S'il n'existe pas, il faut le créer. L'emplacement est donc :

* Windows : `C:\Users\<votre utilisateur>\.Renviron`
* macOS : `/Users/<votre utilisateur>/.Renviron`
* Linux : `/home/<votre utilisateur>/.Renviron`

```R
# .Renviron
AWS_ACCESS_KEY_ID=<identifiant de clé>
AWS_SECRET_ACCESS_KEY=<clé d'accès secrète>
```

## Interface de haut hiveau

`tube` comporte une interface de haut niveau qui permet d'interroger la plateforme à l'aide de fonctions d'analyse de données fournies par le `tidyverse`.

Ces fonctions sont bâties à même une architecture technique décrite dans la section [Interface technique](#interface-technique).

Pour faciliter la découverte des fonctionnalités, les noms de fonction commencent par `ellipse_`. Lorsque `tube` est chargé dans RStudio avec `library(tube)`, taper les lettres `ell` dans la console R ou l'éditeur permet de voir rapidement les fonctions disponibles.

Pour rappel, dans une session R, on peut taper `?<fonction>` pour obtenir de l'aide.

Des efforts sont déployés pour documenter les différentes fonctions fournies par `tube`. Si la documentation n'est pas adéquate, svp [ouvrir une _issue_](https://github.com/ellipse-science/tube/issues) sur ce dépôt pour nous permettre d'améliorer le package pour tout le monde 🙂

### Se connecter

Pour se connecter, utiliser la fonction `ellipse_connect()` :

```R
[ins] r$> library(tube)

[ins] r$> con <- ellipse_connect()
ℹ Pour déconnecter: DBI::dbDisconnect(objet_de_connexion)
```

### Découvrir les données

La première étape de toute analyse est de rencenser les données à notre disposition. C'est le rôle de la fonction `ellipse_discover()`. Elle prend minimalement en paramètre l'objet de connexion obtenu à l'étape précédente :

```r
[ins] r$> con <- ellipse_connect()
ℹ Pour déconnecter: DBI::dbDisconnect(objet_de_connexion)

[ins] r$> ellipse_discover(con)
# A tibble: 13 × 2
   categorie    table
   <chr>        <chr>
 1 Agora+       a-humans
 2 Agora+       a-parliament-debates
 3 Agora+       a-qc-press-releases
 4 Dictionnaire dict-issues
 5 Dictionnaire dict-political-parties-can
 6 Dictionnaire dict-political-parties-qc
 7 Dictionnaire dict-sentiments
 8 Dimension    dim-institutions
 9 Dimension    dim-parliament-members
10 Dimension    dim-parties
11 Radar+       r-media-frontpages
12 Radar+       r-media-headlines
```

Un `tibble` est retourné. On peut y voir les tables qui sont disponibles. En ce moment, les tables retournées sont celles contenues dans l'entrepôt de données (_data warehouse_).

Pour en savoir plus sur une table, on peut simplement la fournir en paramètre comme suit :

```r
[ins] r$> ellipse_discover(con, "a-parliament-debates")
INFO [2024-03-24 21:04:59] [tube::list_glue_tables] listing tables from the datawarehouse
# A tibble: 22 × 4
   table_name           col_name                 col_type is_partition
   <chr>                <chr>                    <chr>    <lgl>
 1 a-parliament-debates institution_id           string   TRUE
 2 a-parliament-debates event_date               date     TRUE
 3 a-parliament-debates id                       string   FALSE
 4 a-parliament-debates event_number             string   FALSE
 5 a-parliament-debates event_title              string   FALSE
 6 a-parliament-debates event_start_time         string   FALSE
 7 a-parliament-debates event_end_time           string   FALSE
 8 a-parliament-debates timestamp                string   FALSE
 9 a-parliament-debates order_of_business_number string   FALSE
10 a-parliament-debates order_of_business_title  string   FALSE
# ℹ 12 more rows
# ℹ Use `print(n = ...)` to see more rows
```

Le concept de _partition_ est important. Le scan d'une table complète peut être très long et croissant selon la grosseur de la table. L'utilisation de partitions permet d'orienter la lecture des données en arrière plan pour lire directement les données souhaitées et ainsi, améliore grandement les performances d'utilisation des données et réduit les coûts d'exploitation (**AWS facture proportionnellement à la quantité de données lues**).

Les jeux de données de la plateforme _Ellipse_ sont partitionnés sur _AWS_, c'est-à-dire que les données d'une table sont regroupées selon les valeurs de certaines variables. Regrouper les données de cette façon permet une efficacité accrue lorsqu'on fait une requête pour utiliser les données. Ainsi, il est recommandé d'utiliser ces variables lorsqu'on veut cibler un sous-ensemble de données. Pour ce faire, il faut connaître les valeurs que peuvent prendre ces variables partitionnées.

Dans l'exemple ci-haut, on voit que `institution_id` et `event_date` sont des variables partitionnées. Pour connaître les valeurs que peuvent prendre ces variables, on peut utiliser la fonction `ellipse_partitions()` :

```r
[ins] r$> parts <- ellipse_partitions(con, "a-parliament-debates")
INFO [2024-03-30 09:53:16] [tube::list_glue_tables] listing tables from the datawarehouse
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)

[ins] r$> parts
# A tibble: 78 × 3
   institution_id event_date       n
   <chr>          <date>     <int64>
 1 CACOMMONS      2007-01-29     245
 2 CACOMMONS      2023-12-12    1764
 3 CACOMMONS      2023-12-13    1872
 4 CACOMMONS      2023-12-14    3392
 5 CACOMMONS      2023-12-15    2384
 6 CACOMMONS      2024-01-29     957
 7 CACOMMONS      2024-01-30    2984
 8 CACOMMONS      2024-01-31    2016
 9 CACOMMONS      2024-02-01    2625
10 CACOMMONS      2024-02-02     805
# ℹ 68 more rows
# ℹ Use `print(n = ...)` to see more rows
```

Un `tibble` est retourné. Chacune des lignes représente une combinaison de valeurs des variables partitionnées de la table, ainsi que le nombre d'observations associées.

Ces valeurs peuvent nous guider dans nos requêtes subséquentes. À l'usage, pour obtenir une partie des données, on remarquera que l'utilisation d'un filtre sur des variables partionnées sera beaucoup plus rapide que sur des variables non-partitionnées. Il est donc recommandé d'utiliser les filtres de variables partitionnées en premier puis ceux sur les variables non-partionnées pour raffiner.

Comme il s'agit d'un `tibble` ordinaire, on peut l'explorer avec les fonctions habituelles de `dplyr`:

```r
[ins] r$> dplyr::distinct(parts, institution_id)
# A tibble: 3 × 1
  institution_id
  <chr>
1 CACOMMONS
2 EUPARL
3 QCASSNAT

[ins] r$> dplyr::filter(parts, institution_id == "EUPARL") |> print(n = 30)
# A tibble: 22 × 3
   institution_id event_date       n
   <chr>          <date>     <int64>
 1 EUPARL         2023-10-04     447
 2 EUPARL         2023-12-11     174
 3 EUPARL         2023-12-12     479
 4 EUPARL         2023-12-13     416
 5 EUPARL         2023-12-14     127
 6 EUPARL         2024-01-15     180
 7 EUPARL         2024-01-16     428
 8 EUPARL         2024-01-17     507
 9 EUPARL         2024-01-18     165
10 EUPARL         2024-01-25       5
11 EUPARL         2024-02-05     182
12 EUPARL         2024-02-06     470
13 EUPARL         2024-02-07     479
14 EUPARL         2024-02-08     113
15 EUPARL         2024-02-26     215
16 EUPARL         2024-02-27     468
17 EUPARL         2024-02-28     421
18 EUPARL         2024-02-29     138
19 EUPARL         2024-03-11     196
20 EUPARL         2024-03-12     430
21 EUPARL         2024-03-13     417
22 EUPARL         2024-03-14     170
```

### Interroger les données

Maintenant qu'on a une idée des données qui nous intéressent et de la façon dont elles sont partitionnées, on peut les interroger.

La fonction `ellipse_query()` nous retourne un objet qui est exploitable avec `dplyr`.

#### Pipeline des débats parlementaires

N'est-il pas intéressant d'étudier les termes proscrits à l'assemblée nationale?

```r
[nav] r$> df <-
            ellipse_query(con, "a-parliament-debates") |>
            dplyr::filter(institution_id == "QCASSNAT", event_date == "2024-02-22") |>
            dplyr::collect()
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 1.03 MB)

[ins] r$> df |>
            dplyr::filter(stringr::str_detect(intervention_text, "fligne")) |>
            dplyr::distinct(intervention_number, speaker_full_name, intervention_text)
# A tibble: 4 × 3
  intervention_number speaker_full_name intervention_text
  <chr>               <chr>             <chr>
1 371267-67           Marc Tanguay      Bien, Mme la Présidente, c'est une chose d'avoir les normes les plus sévères, puis c'en est une autre de décider de ne pas les app…
2 371267-70           La Présidente     Je vous demande de faire très attention. Il y a un «fligne-flagne» pour d'autres sujets, dans le lexique, et vous le savez. Demeur…
3 371267-71           M. Jolin-Barrette Mme la Présidente, «le fligne-flagne dans les garderies libérales» est à l'index. Alors, je pense, Mme la Présidente...
4 371267-73           M. Jolin-Barrette ...je ne pense pas qu'on n'a pas le droit de dire «garderie» ici, mais le terme «fligne-flagne» est proscrit.
```

#### Pipeline des unes des médias

On peut, par exemple, rechercher les titres des unes d'un média pour une journée particulière.

```r
[ins] r$> ellipse_discover(con, "r-media-headlines")
INFO [2024-03-30 10:21:04] [tube::list_glue_tables] listing tables from the datawarehouse
# A tibble: 9 × 4
  table_name        col_name               col_type is_partition
  <chr>             <chr>                  <chr>    <lgl>
1 r-media-headlines date                   date     TRUE
2 r-media-headlines media_id               string   TRUE
3 r-media-headlines id                     string   FALSE
4 r-media-headlines extraction_datetime    string   FALSE
5 r-media-headlines title                  string   FALSE
6 r-media-headlines author_id              string   FALSE
7 r-media-headlines body                   string   FALSE
8 r-media-headlines metadata_lake_item_key string   FALSE
9 r-media-headlines metadata_url           string   FALSE

[ins] r$> ellipse_partitions(con, "r-media-headlines")
INFO [2024-03-30 10:21:51] [tube::list_glue_tables] listing tables from the datawarehouse
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
# A tibble: 128 × 3
   date       media_id       n
   <date>     <chr>    <int64>
 1 2023-10-23 TVA           24
 2 2024-01-28 TVA           59
 3 2024-01-29 RCI            7
 4 2024-01-29 TVA          126
 5 2024-01-30 RCI          143
 6 2024-01-30 TVA          142
 7 2024-01-31 RCI          144
 8 2024-01-31 TVA          148
 9 2024-02-01 RCI          144
10 2024-02-01 TVA          136
# ℹ 118 more rows
# ℹ Use `print(n = ...)` to see more rows

[ins] r$> df <-
            ellipse_query(con, "r-media-headlines") |>
            dplyr::filter(date == as.Date("2024-01-30"), media_id == "RCI") |>
            dplyr::collect()
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 526.42 KB)

[ins] r$> df |>
            dplyr::mutate(date_heure = lubridate::as_datetime(extraction_datetime,
                                                              tz = "America/New_York")) |>
            dplyr::distinct(date_heure, title)
Date in ISO8601 format; converting timezone from UTC to "America/New_York".
# A tibble: 143 × 2
   date_heure          title
   <dttm>              <chr>
 1 2024-01-31 02:34:33 Demandeurs d’asile : Ottawa ne répondra pas à toutes les demandes du Québec | Radio-Canada
 2 2024-01-30 18:04:33 Scandale de Hockey Canada :  McLeod et Dubé parmi les cinq joueurs accusés | Radio-Canada
 3 2024-01-30 23:54:33 Demandeurs d’asile : Ottawa ne répondra pas à toutes les demandes du Québec | Radio-Canada
 4 2024-01-31 03:24:34 Demandeurs d’asile : Ottawa ne répondra pas à toutes les demandes du Québec | Radio-Canada
 5 2024-01-30 16:44:34 Le nucléaire devrait faire partie de la solution après 2035, dit Hydro-Québec | Radio-Canada
 6 2024-01-30 15:04:33 Québec annonce 200 millions $ en allocations personnalisées aux RPA | Radio-Canada
 7 2024-01-30 17:54:33 Scandale de Hockey Canada :  McLeod et Dubé parmi les cinq joueurs accusés | Radio-Canada
 8 2024-01-31 00:54:33 Demandeurs d’asile : Ottawa ne répondra pas à toutes les demandes du Québec | Radio-Canada
 9 2024-01-30 19:04:33 Scandale de Hockey Canada : l’identité de 4 des 5 joueurs accusés est confirmée | Radio-Canada
10 2024-01-30 23:34:33 Demandeurs d’asile : Ottawa ne répondra pas à toutes les demandes du Québec | Radio-Canada
# ℹ 133 more rows
# ℹ Use `print(n = ...)` to see more rows
```

Les verbes `dplyr` disponibles sont limités sur une source distante comme la plateforme _Ellipse_. Une fois qu'on a une idée des données que l'on veut, on peut envoyer une requête qui filtre sur une plage de valeurs pertinentes pour les partitions présentes, puis utiliser la fonction `dplyr::collect()` pour ramener les données localement. Après ceci, toute la fonctionnalité de manipulation de données de R et du _tidyverse_ sont disponibles pour traiter les données.

## Interface technique

L'interface technique de `tube` reflète l'architecture ETL de la plateforme de données d'_Ellipse_.

Les fonctions exportées commencent par :

* `get_`
* `list_`
* `put_`
* `update_`

Elles requièrent en général les informations d'identification obtenues via la fonction `aws_session()`.

Cette interface est toute indiquée pour l'écriture de raffineurs. Plusieurs exemples de son utilisation sont disponibles dans le dépôt [ellipse-science/aws-refiners](https://github.com/ellipse-science/aws-refiners), plus particulierèment sous [refiners/examples](https://github.com/ellipse-science/aws-refiners/blob/main/refiners/examples/examples.R).


Pour la documentation conceptuelle de la plateforme de données du CAP, voir le répertoire [doc](https://github.com/ellipse-science/tube/doc)