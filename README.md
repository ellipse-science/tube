# tube 🚰

![Le tube](tube.png)
(Image par [Dall-E 3](https://openai.com/dall-e-3))

Interface R d'accès à la plateforme de données _Ellipse_.

## Pré-requis

### Installation
Pour installer ce package, utilisez la commande `remotes::install_github("ellipse-science/tube")`  ou `devtools::install_github("ellipse-science/tube")`.  Si les packages dépendants ne sont pas à jour, SVP assurez-vous de chois l'option 1 pour tous les mettre à jour.

### Clé d'accès
Pour accéder aux données de la plateforme, il faut configurer les clés d'accès AWS. Pour obtenir ces informations, contacter Patrick Poncet (@patoscope) sur Slack.

Ensuite, il faut les ajouter au fichier `~/.Renviron` qui est chargé au démarrage de la session R. Ce fichier réside à la racine de votre répertoire d'utilisateur. S'il n'existe pas, il faut le créer ou le modifier avec la commande `usethis::edit_r_environ()`. Selon votre plateforme, l'emplacement du fichier est :

* Windows : `C:\Users\<votre utilisateur>\.Renviron`
* macOS : `/Users/<votre utilisateur>/.Renviron`
* Linux : `/home/<votre utilisateur>/.Renviron`

### Environnements
Il existe deux environnements (deux copies non identiques) de la plateforme de données sur AWS.

* Une copie de développement (DEV) dans laquelle on développe les pipelines et où on conçoit la structure des données (tables, variables etc.).  Vous allez principalement vous connecter en DEV pour valider le travail des développeurs et la structure de données que leurs pipelines ont générée, en faisant des tests les plus réels possible selon vos projets de recherche, sur des petits échantillons de données.  vous pourries aussi utiliser l'environnement de DEV pour valider des dictionnaires ou des dimensions.
* Une copie de PROD: Lorsqu'on est satisfait avec la conception, on passe en production (PROD). Là les données sont officielles, de qualité en tout temps, dans leur structure approuvée (par vous en DEV).

Un cas d'usage très fréquent sera de vous connecter en PROD pour obtenir les données récentes, à jour, et en DEV pour publier des dataframes issus de vos travaux d'analyse dans R et valider leur qualité avant de les publier en PROD.  Nous avons plus de détails sur ça plus loin.

Pour plus de détails sur les environnements, voir le [diagramme descriptif des environnements de la plateforme `Ellipse`](https://github.com/ellipse-science/tube-doc/blob/main/ellipse-dev-prod.drawio.png).

Pour se connecter à l'un et/ou l'autre des environnements, il faut le choisir au moment de la connection.  Pour cela, il faut configurer 2 paires "ID de clé"+"Secret de clé" comme suit:

```R
# .Renviron
AWS_ACCESS_KEY_ID_DEV=<identifiant de clé en DEV>
AWS_SECRET_ACCESS_KEY_DEV=<clé d''accès secrète en DEV>
AWS_ACCESS_KEY_ID_PROD=<identifiant de clé en PROD>
AWS_SECRET_ACCESS_KEY_PROD=<clé d''accès secrète en PROD>
```

Pour éditer le fichier .Renviron tel qu'illustré ci-dessus, simplement lancer la commande `usethis::edit_r_environ()` dans votre console R.  Modifiez le fichier, enregistrez-le et redémarrez votre session R

C'est au moment de la connexion à la plateforme dans votre code R que vous devez choisir à quel environnement vous voulez vous connecter, comme suit:

```R
tube::ellipse_connect(env = "DEV", database = "datawarehouse")
```

Additionnellement, comme le montre la commande ci-dessus, il vous faut spécifier si votre connexion doit se faire sur l'entrepôt de données (datawarehouse) ou sur les comptoirs de données (datamarts).  Pour plus d'explications sur ces concepts, veuillez vous référer au repo [`tube-doc`](https://github.com/ellipse-science/tube-doc/tree/main) dans lequel on décrit [les trois composantes principales d'une platformes de données](https://github.com/ellipse-science/tube-doc/blob/main/ellipse-datalake-datawarehouse-datamart.drawio.png).

## Interface de haut hiveau

`tube` comporte une interface de haut niveau qui permet d'interroger la plateforme à l'aide de fonctions d'analyse de données fournies par le `tidyverse`.

Pour faciliter la découverte des fonctionnalités, les noms de fonction commencent par `ellipse_`. Lorsque `tube` est chargé dans RStudio avec `library(tube)`, taper les lettres `ell` dans la console R ou l'éditeur permet de voir rapidement les fonctions disponibles.

Pour rappel, dans une session R, on peut taper `?<fonction>` pour obtenir de l'aide.

Des efforts sont déployés pour documenter les différentes fonctions fournies par `tube`. Si la documentation n'est pas adéquate, svp [ouvrir une _issue_](https://github.com/ellipse-science/tube/issues) sur ce dépôt pour nous permettre d'améliorer le package pour tout le monde 🙂

### Se connecter

Pour se connecter, utiliser la fonction `ellipse_connect()`. Le seul paramètre obligatoire est l'environnement (`DEV` ou `PROD`) :

```R
r$> con <- ellipse_connect(env = "PROD", database = "datawarehouse")
ℹ Environnement: PROD
ℹ Database: datawarehouse
ℹ Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)
ℹ Base de données: gluestackdatawarehousedbe64d5725
✔ Connexion établie avec succès! 👍
```

### Découvrir les données

La première étape de toute analyse est de rencenser les données à notre disposition. C'est le rôle de la fonction `ellipse_discover()`. Elle prend minimalement en paramètre l'objet de connexion obtenu à l'étape précédente :

```r
[ins] r$> con <- ellipse_connect(env = "DEV", database = "datawarehouse")
ℹ Environnement: DEV
ℹ Database: datawarehouse
ℹ Pour déconnecter: DBI::dbDisconnect(objet_de_connexion)

[ins] r$> ellipse_discover(con)
# A tibble: 20 × 2
   categorie    table
   <chr>        <chr>
 1 Agora+       a-ca-parliament-debates
 2 Agora+       a-ca-press-releases
 3 Agora+       a-eu-parliament-debates
 4 Agora+       a-humans
 5 Agora+       a-qc-parliament-debates
 6 Agora+       a-qc-press-releases
 7 Dictionnaire dict-issues
 8 Dictionnaire dict-political-parties-can
 9 Dictionnaire dict-political-parties-qc
10 Dictionnaire dict-sentiments
11 Dimension    dim-institutions
12 Dimension    dim-medias
13 Dimension    dim-parliament-members
14 Dimension    dim-parties
15 Radar+       r-factiva
16 Radar+       r-media-frontpages
17 Radar+       r-media-headlines
18 Autre        test-datamart-csv_unprocessed
19 Autre        test2-datamart_partition1_unprocessed
20 Autre        test2-datamart_partition2_unprocessed
```

Un `tibble` est retourné. On peut y voir les tables qui sont disponibles. En ce moment, les tables retournées sont celles contenues dans l'entrepôt de données (_data warehouse_).

Pour en savoir plus sur une table, on peut simplement la fournir en paramètre comme suit :

```r
[ins] r$> ellipse_discover(con, "a-qc-parliament-debates")
INFO [2024-06-11 21:15:34] [tube::list_glue_tables] listing tables from the datawarehouse
# A tibble: 21 × 4
   table_name              col_name                 col_type is_partition
   <chr>                   <chr>                    <chr>    <lgl>
 1 a-qc-parliament-debates event_date               date     TRUE
 2 a-qc-parliament-debates id                       string   FALSE
 3 a-qc-parliament-debates institution_id           string   FALSE
 4 a-qc-parliament-debates event_number             string   FALSE
 5 a-qc-parliament-debates event_title              string   FALSE
 6 a-qc-parliament-debates event_start_time         string   FALSE
 7 a-qc-parliament-debates event_end_time           string   FALSE
 8 a-qc-parliament-debates timestamp                string   FALSE
 9 a-qc-parliament-debates order_of_business_number string   FALSE
10 a-qc-parliament-debates order_of_business_title  string   FALSE
# ℹ 11 more rows
# ℹ Use `print(n = ...)` to see more rows
```

Le concept de _partition_ est important. Le scan d'une table complète peut être très long et croissant selon la grosseur de la table. L'utilisation de partitions permet d'orienter la lecture des données en arrière plan pour lire directement les données souhaitées et ainsi, améliore grandement les performances d'utilisation des données et réduit les coûts d'exploitation (**AWS facture proportionnellement à la quantité de données lues**).

Les jeux de données de la plateforme _Ellipse_ sont partitionnés sur _AWS_, c'est-à-dire que les données d'une table sont regroupées selon les valeurs de certaines variables. Regrouper les données de cette façon permet une efficacité accrue lorsqu'on fait une requête pour utiliser les données. Ainsi, il est recommandé d'utiliser ces variables lorsqu'on veut cibler un sous-ensemble de données. Pour ce faire, il faut connaître les valeurs que peuvent prendre ces variables partitionnées.

Dans l'exemple ci-haut, on voit que `event_date` est une variable partitionnée. Pour connaître les valeurs que peuvent prendre ces variables, on peut utiliser la fonction `ellipse_partitions()` :

```r
[ins] r$> parts <- ellipse_partitions(con, "a-qc-parliament-debates")
INFO [2024-06-11 21:18:12] [tube::list_glue_tables] listing tables from the datawarehouse
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)

[ins] r$> print(parts)
# A tibble: 10 × 2
   event_date       n
   <date>     <int64>
 1 2024-05-21     262
 2 2024-05-22     309
 3 2024-05-23     262
 4 2024-05-28     263
 5 2024-05-29     320
 6 2024-05-30     259
 7 2024-05-31     223
 8 2024-06-04     305
 9 2024-06-05     293
10 2024-06-06     315
```

Un `tibble` est retourné. Chacune des lignes représente une combinaison de valeurs des variables partitionnées de la table, ainsi que le nombre d'observations associées.

Ces valeurs peuvent nous guider dans nos requêtes subséquentes. À l'usage, pour obtenir une partie des données, on remarquera que l'utilisation d'un filtre sur des variables partionnées sera beaucoup plus rapide que sur des variables non-partitionnées. Il est donc recommandé d'utiliser les filtres de variables partitionnées en premier puis ceux sur les variables non-partionnées pour raffiner.

Comme il s'agit d'un `tibble` ordinaire, on peut l'explorer avec les fonctions habituelles de `dplyr`:

```r
[ins] r$> dplyr::filter(parts, n > 300)
# A tibble: 4 × 2
  event_date       n
  <date>     <int64>
1 2024-05-22     309
2 2024-05-29     320
3 2024-06-04     305
4 2024-06-06     315
```

### Interroger les données

#### Pipeline des débats parlementaires

Maintenant qu'on a une idée des données qui nous intéressent et de la façon dont elles sont partitionnées, on peut les interroger.

La fonction `ellipse_query()` nous retourne un objet qui est exploitable avec `dplyr`.

N'est-il pas intéressant d'étudier les interventions du premier ministre à l'assemblée nationale?

```r
[nav] df <-
        tube::ellipse_query(con, "a-qc-parliament-debates") |>
        dplyr::filter(event_date == "2024-05-23") |>
        dplyr::collect()
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 100.45 KB)

[ins] r$> df |>
            dplyr::filter(stringr::str_detect(speaker, "Legault")) |>
            dplyr::distinct(intervention_number, speaker, intervention_text)
# A tibble: 17 × 3
   intervention_number speaker          intervention_text                                                                                                 
   <chr>               <chr>            <chr>                                                                                                             
 1 380411-36           François Legault "Oui, Mme la Présidente. Bon, d'abord, c'est une de mes grandes fiertés, avec le ministre de l'Économie, d'avoir …
 2 380411-40           François Legault "Mme la Présidente, d'abord, je veux rassurer tout le monde, le ministre de l'Économie, il n'est pas sortant, là,…
 3 380411-42           M. Legault       "...les salaires les plus élevés."                                                                                
 4 380411-51           François Legault "Oui, Mme la Présidente, le chef de l'opposition officielle n'a pas été gentil avec moi en fin de semaine. Là, il…
 5 380411-56           François Legault "Mme la Présidente, quand le gouvernement libéral était au pouvoir, il y avait des tarifs privilégiés pour les en…
 6 380411-90           François Legault "Oui, Mme la Présidente, le Parti libéral est un parti très courageux. Quand il rencontre les gens de Rivière-du-…
 7 380411-99           M. Legault       "Donc, aujourd'hui, le Parti libéral, étant donné qu'il y a des gens de Rivière-du-Loup, bien, propose que la tra…
 8 380411-121          François Legault "Mme la Présidente, d'abord, c'est important de le répéter, puis, avec raison, la vice-première ministre le répèt…
 9 380411-125          François Legault "Oui. Mme la Présidente, je sais que ça n'intéresse pas beaucoup Québec solidaire, l'économie, mais, quand on reg…
10 380411-129          François Legault "Oui. Je note deux choses, Mme la Présidente. D'abord, Québec solidaire préférerait qu'on électrifie les boeufs a…
11 380411-151          François Legault "Oui, Mme la Présidente, c'est vrai depuis tous les rapports qui ont été déposés, entre autres le rapport de Mich…
12 380411-155          François Legault "Oui, Mme la Présidente, ce qui est important, puis l'objectif, c'est qu'il y ait plus de Québécois qui soient pr…
13 380411-157          M. Legault       "...par une infirmière. C'est ce qu'on fait."                                                                     
14 380411-161          François Legault "Oui, Mme la Présidente. Bien, d'abord, on a déjà revu le mode de rémunération, c'était dans une entente qui se t…
15 380411-208          M. Legault       "M. le Président, je propose, après consultation auprès des partis d'opposition et des députés indépendants :\n«Q…
16 380411-213          M. Legault       "Oui. M. le Président, je propose, après consultation auprès des partis de l'opposition et des députés indépendan…
17 380411-36           François Legault "Oui, Mme la Présidente. Bon, d'abord, c'est une de mes grandes fiertés, avec le ministre de l'Économie, d'avoir …
```

#### Pipeline des unes des médias

On peut, par exemple, rechercher les titres des unes d'un média pour une journée particulière.

```r
[ins] r$> ellipse_discover(con, "r-media-headlines")
INFO [2024-03-30 10:21:04] [tube::list_glue_tables] listing tables from the datawarehouse
# A tibble: 13 × 4
   table_name        col_name               col_type is_partition
   <chr>             <chr>                  <chr>    <lgl>       
 1 r-media-headlines extraction_year        int      TRUE        
 2 r-media-headlines extraction_month       int      TRUE        
 3 r-media-headlines extraction_day         int      TRUE        
 4 r-media-headlines media_id               string   TRUE        
 5 r-media-headlines id                     string   FALSE       
 6 r-media-headlines extraction_date        date     FALSE       
 7 r-media-headlines extraction_time        string   FALSE       
 8 r-media-headlines publish_date           date     FALSE       
 9 r-media-headlines title                  string   FALSE       
10 r-media-headlines author                 string   FALSE       
11 r-media-headlines body                   string   FALSE       
12 r-media-headlines metadata_url           string   FALSE       
13 r-media-headlines metadata_lake_item_key string   FALSE  

[ins] r$> ellipse_partitions(con, "r-media-headlines")
INFO [2024-03-30 10:21:51] [tube::list_glue_tables] listing tables from the datawarehouse
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
# A tibble: 715 × 5
   extraction_year extraction_month extraction_day media_id       n
             <int>            <int>          <int> <chr>    <int64>
 1            2024                5             28 CBC           27
 2            2024                5             28 CTV           27
 3            2024                5             28 GAM           27
 4            2024                5             28 GN            27
 5            2024                5             28 JDM           30
 6            2024                5             28 LAP           27
 7            2024                5             28 LED           27
 8            2024                5             28 MG            27
 9            2024                5             28 NP            30
10            2024                5             28 RCI           30
# ℹ 705 more rows
# ℹ Use `print(n = ...)` to see more rows

[ins] r$> df <-
            ellipse_query(con, "r-media-headlines") |>
            dplyr::filter(extraction_year == 2024, extraction_month == 5, media_id == "RCI") |>
            dplyr::collect()
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 526.42 KB)

[ins] r$> df |>
            dplyr::mutate(
              date_heure = lubridate::as_datetime(paste(extraction_date, extraction_time),
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

### Croiser des données

1. Aller chercher les médias dans l'entrepôt de données en DEV

Les médias, comme les autres données de références du CAPP, sont ce qu'on appelle des données dimensionnelles.  Pour plus d'information sur les données dimensionnelles, veuillez consulter le [README du dépôt tube-dimensions](https://github.com/ellipse-science/tube-dimensions/blob/main/README.md), le [diagramme de flux de travail organique des dimensions](https://github.com/ellipse-science/tube-doc/blob/main/dimensions-workflow-organique.drawio.png) ainsi que le [diagramme de flux de travail organisationnel des dimensions](https://github.com/ellipse-science/tube-doc/blob/main/dimensions-workflow-organisationnel.drawio.png).

```r
[ins] r$> condwd <- tube::ellipse_connect("DEV", "datawarehouse")
ℹ Environnement: DEV
ℹ Database: datawarehouse
ℹ Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)
ℹ Base de données: gluestackdatawarehousedbe64d5725
✔ Connexion établie avec succès! 👍

r$> df_medias <- tube::ellipse_query(condwd, "dim-medias") |>
      dplyr::collect()
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 4.98 KB)
```

1. Aller chercher les Unes des médias dans l'entrepôt de données en PROD
```r
[ins] r$> condwp <- tube::ellipse_connect("PROD", "datawarehouse")
ℹ Environnement: PROD
ℹ Database: datawarehouse
ℹ Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)
ℹ Base de données: gluestackdatawarehousedbe64d5725
✔ Connexion établie avec succès! 👍

r$> df_headlines <- tube::ellipse_query(condwp, "r-media-headlines") |>
      dplyr::filter(extraction_year == 2024 & extraction_month == 7 & extraction_day == 22) |>
      dplyr::collect()
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 9.95 MB)
```

À ce stade nous avons deux dataframe.  Pour les croiser l'un avec l'autre, il faut qu'ils aient deux colonnes qui contiennent les mêmes valeurs standardisées.  Validons que c'est bien le cas.

```r
[ins] r$> colnames(df_medias)
 [1] "id"                     "long_name"              "short_name"             "other_names"            "lang"
 [6] "country_id"             "province_or_state"      "x_handle"               "web_site"               "start_date"
[11] "end_date"               "wikipedia_qid"          "wikipedia_url"          "metadata_lake_item_key" "metadata_url"
[16] "version"

r$> colnames(df_headlines)
 [1] "id"                     "extraction_date"        "extraction_time"        "publish_date"           "title"
 [6] "author"                 "body"                   "metadata_url"           "metadata_lake_item_key" "extraction_year"
[11] "extraction_month"       "extraction_day"         "media_id"
```

On va pouvoir joindre les deux datframes sur la colonne `id` de `df_medias` et `media_id` de `df_headlines`

```r
[ins] r$> df <- dplyr::inner_join(df_medias, df_headlines, by = c("id" = "media_id")) |>
      dplyr::select(id, province_or_state, title, body, extraction_date)

r$> head(df)
# A tibble: 6 × 5
  id    province_or_state title                                                                                        body  extraction_date
  <chr> <chr>             <chr>                                                                                        <chr> <date>
1 TVA   QC                EN DIRECT | Suivez les derniers développements sur le retrait de Joe Biden à la course à la… "Joe… 2024-07-22
2 TVA   QC                EN DIRECT | Suivez les derniers développements sur le retrait de Joe Biden à la course à la… "Joe… 2024-07-22
3 TVA   QC                EN DIRECT | Suivez les derniers développements sur le retrait de Joe Biden à la course à la… "Joe… 2024-07-22
4 TVA   QC                EN DIRECT | Suivez les derniers développements sur le retrait de Joe Biden à la course à la… "Joe… 2024-07-22
5 TVA   QC                EN DIRECT | Suivez les derniers développements sur le retrait de Joe Biden à la course à la… "Joe… 2024-07-22
6 TVA   QC                EN DIRECT | Suivez les derniers développements sur le retrait de Joe Biden à la course à la… "Joe… 2024-07-22
```

### Publier un jeu de données dans un datamart

Pour plus de détails sur les concepts de datalake, datawarehouse, datamarts, voir [les trois composantes principales d'une platformes de données](https://github.com/ellipse-science/tube-doc/blob/main/ellipse-datalake-datawarehouse-datamart.drawio.png)


Pour publier notre nouveau jeu de données dans un datamart, on peut utiliser la fonction `tube::ellipse_publish()`.

```r
[ins] 

# Connexion au datamarts en DEV
r$> condmd <- tube::ellipse_connect("DEV", "datamarts")
ℹ Environnement: DEV
ℹ Database: datamarts
ℹ Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)
ℹ Base de données: gluestackdatamartdbd046f685
✔ Connexion établie avec succès! 👍

# publication de la table nommée headlinesbyprovinces dans le datamart nommé myradardatamart
# avec le contenu du dataframe df, dans la base de données des datamarts en DEV
r$> tube::ellipse_publish(con = condmd,
      dataframe = df,
      datamart = "myradardatamart",
      table = "headlinesbyprovinces",
      tag = "headlines_du_22_juillet_2024")

✖ Le datamart fourni n'existe pas! 😅
❓Voulez-vous créer un nouveau datamart? (oui/non) oui
ℹ Création du datamart en cours...
✖ La table demandée n'existe pas

❓Voulez-vous créer la table? (oui/non) oui
ℹ Création de la table en cours...
✔ La table a été créée avec succès.

❓Voulez-vous traiter les données maintenant pour les rendre disponibles immédiatement?
  Si vous ne le faites pas maintenant, le traitement sers déclenché automatiquement dans les 6 prochaines heures.
  Votre choix (oui/non) oui
✔ Le traitement des données a été déclenché avec succès.
ℹ Les données seront disponibles dans les prochaines minutes
ℹ N'oubliez pas de vous déconnecter de la plateforme ellipse avec `ellipse_disconnect(...)` 👋.
```

### Injecter des données brutes dans la plateforme Ellipse
La règle c'est que l'injection de données dans Ellipse est automatisée de bout-en bout:  Les données sont extraites depuis les internets à un intervalle défini. Elles sont entreposées dans le lac de données telles quelles.  Et ensuite elles sont transformées sous forme tabulaire et stockées dans l'entrepôt de données où vous pouvez y accéder avec les fonctions `ellipse_*`.  Il y a 3 exceptions à cette règle:

#### L'obtention des données brutes n'est pas automatisable
Dans certains cas, malheureusement, il n'est pas possible d'extraire les données brutes automatiquement.  Il faut qu'une personne se charge de collecter les fichiers contenant les données brutes et les pousse dans la plateforme de données Ellipse.  Pour cela il faut qu'un pipeline de données semi-automatisé ait été développé préalablement par un développeur.  C'est le cas notamment des articles de presse disponibles dans la banque de données Factiva, de certains sondages (qualtrics) etc.  Pour comprendre le flus de travail relié à ce type d'acquisition et traitement des données, veuilles vous référer à [ce diagramme](https://github.com/ellipse-science/tube-doc/blob/main/pipeline_semi_automatise.drawio.png).  Pour toute question ou imprécision, n'hésitez pas à ouvrir une issue dans ce dépôt.

#### Il s'agit de données dimensionnelles
Les données dimensionnelles sont les données de références du CAPP.  Elles représentent les axes sur lesquelles nous sommes typiquement intéressés à porter nos analyses.  Par exemple, le ton des députés de l'Assemblée nationale du Québec **par parti** au fil du temps ou sur des **enjeux** précis.  Pour cela il nous fait croiser des données factuelles (les interventions des députés) avec les données dimensionnelles (les députés et leur attribut `party`) et un dictionnaire d'enjeux (voir prochaine section).

Lorsqu'on construire nos données de références reviens à construire les dimensions (des entités comme partis, médias, cisconscriptions etc).  Cela se fait en créant des CSV et en les injectant dans `Ellipse`.  Ensuite il faut les entretenir en fonction de l'évolution des choses (p.ex. si un député change de parti ou qu'il n'est pas réélu) et les réinjecter dans la plateforme.

Cela se fait via des pipelines de données dimensionnelles.  Pour plus d'information veuillez lire le [README du dépôt tube-dimensions](https://github.com/ellipse-science/tube-dimensions) et vous référer à aux diagrammes de flux de travail qui décrivent l'injection de données dimentionnelles dans `Ellipse`:  [ici](https://github.com/ellipse-science/tube-doc/blob/main/dimensions-workflow-organique.drawio.png) et [ici](https://github.com/ellipse-science/tube-doc/blob/main/dimensions-workflow-organisationnel.drawio.png).

#### Il s'agit de dictionnaires
Les dictionnaires sont des données dimensionnelles (dans le sens qu'elles sont des données de références du CAPP).  Elles sont dédiées à l'analyse textuelles.  Nos dictionnaires sont construits de toute pièce, manuellement, via des techniques particulières.  Pour plus de détails sur les dictionnaires, veuillez consultes le [README du dépôt tube-dictionaries]([https://github.com/ellipse-science/tube-dictionaries/tree/main](https://github.com/ellipse-science/tube-dictionaries/blob/develop/README.md).

### Notes sur dplyr

Les verbes `dplyr` disponibles sont limités sur une source distante comme la plateforme _Ellipse_. Une fois qu'on a une idée des données que l'on veut, on peut envoyer une requête qui filtre sur une plage de valeurs pertinentes pour les partitions présentes, puis utiliser la fonction `dplyr::collect()` pour ramener les données localement. Après ceci, toute la fonctionnalité de manipulation de données de R et du _tidyverse_ sont disponibles pour traiter les données.

## Development Setup

### For Package Contributors

This package follows strict development guidelines with comprehensive testing and quality assurance.

#### Required Environment Variables

The package requires AWS credentials in your `.Renviron` file:

```
AWS_ACCESS_KEY_ID_DEV=<your dev access key>
AWS_SECRET_ACCESS_KEY_DEV=<your dev secret key>
AWS_ACCESS_KEY_ID_PROD=<your prod access key>
AWS_SECRET_ACCESS_KEY_PROD=<your prod secret key>
AWS_REGION=ca-central-1
```

#### Development Workflow

1. **Clone and setup**:
   ```r
   # Install development dependencies
   remotes::install_dev_deps()
   install.packages(c("lintr", "covr", "testthat", "mockery"))
   ```

2. **Quality Assurance Scripts**:
   ```r
   # Load QA functions
   source("dev-qa-scripts.R")
   
   # Quick check during development
   quick_check()
   
   # Full QA pipeline before commits
   qa_pipeline()
   ```

3. **Pre-commit Hooks** (recommended):
   ```bash
   pip install pre-commit
   pre-commit install
   ```

#### Testing Requirements

- **100% test coverage required** - no exceptions
- All tests must pass before any commits
- Tests use mocked AWS services to avoid real API calls
- Environment variables are mocked for testing

#### Code Quality Standards

- **All code must pass `lintr::lint_package()`**
- Follow existing `.lintr` configuration
- Use roxygen2 documentation for all exported functions
- Follow consistent naming conventions (snake_case)

#### CI/CD Pipeline

GitHub Actions automatically runs:
- Linting checks (must pass)
- Full test suite (must pass) 
- Test coverage analysis (95%+ required)
- R CMD check (must pass)

#### Contributing

1. Create feature branch from `main`
2. Implement changes with full test coverage
3. Run `qa_pipeline()` locally
4. Submit pull request
5. All CI/CD checks must pass

For conceptual documentation, see [tube-doc](https://github.com/ellipse-science/tube-doc/tree/main)
