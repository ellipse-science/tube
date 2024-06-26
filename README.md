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

Il existe deux environnement (deux copies non identiques) de la plateforme de données sur AWS.

* Une copie de développement (DEV) dans laquelle on développe les pipelines et où on conçoit les structure des données (tables, variables etc.).  Vous allez principalement vous connecter en DEV pour valider le travail des développeurs et la structure de données que leur pipelines va générer, en faisant des tests les plus réels possible selon vos projets de recherche, sur des petits échantillons de données.
* Une copie de PROD: Lorsqu'on est satisfait avec la conception, on passe en production (PROD). Là les données sont officielles, de qualité en tout temps, dans leur structure approuvée (par vous en DEV).

Pour se connecter à l'un ou l'autre des environnements, il faut le choisir au moment de la connection.  Pour cela, il faut configurer 2 paires "ID de clé"+"Secret de clé" comme suit:

```R
# .Renviron
AWS_ACCESS_KEY_ID_DEV=<identifiant de clé en DEV>
AWS_SECRET_ACCESS_KEY_DEV=<clé d''accès secrète en DEV>
AWS_ACCESS_KEY_ID_PROD=<identifiant de clé en PROD>
AWS_SECRET_ACCESS_KEY_PROD=<clé d''accès secrète en PROD>
```

Pour éditer le fichier .Renviron tel qu'illustré ci-dessus, simplement lancer la commande `use_this::edit_r_environ()` dans votre console R.  Modifiez le fichier, enregistrez-le et redémarrez votre session R

C'est au moment de la connexion à la plateforme dans cotre code R que vous devez choisir à quel environnement vous voulez vous connecter, comme suit:

```R
tube::ellipse_connect("DEV")
```

## Interface de haut hiveau

`tube` comporte une interface de haut niveau qui permet d'interroger la plateforme à l'aide de fonctions d'analyse de données fournies par le `tidyverse`.

Pour faciliter la découverte des fonctionnalités, les noms de fonction commencent par `ellipse_`. Lorsque `tube` est chargé dans RStudio avec `library(tube)`, taper les lettres `ell` dans la console R ou l'éditeur permet de voir rapidement les fonctions disponibles.

Pour rappel, dans une session R, on peut taper `?<fonction>` pour obtenir de l'aide.

Des efforts sont déployés pour documenter les différentes fonctions fournies par `tube`. Si la documentation n'est pas adéquate, svp [ouvrir une _issue_](https://github.com/ellipse-science/tube/issues) sur ce dépôt pour nous permettre d'améliorer le package pour tout le monde 🙂

### Se connecter

Pour se connecter, utiliser la fonction `ellipse_connect()`. Le seul paramètre obligatoire est l'environnement (`DEV` ou `PROD`) :

```R
[ins] r$> library(tube)

[ins] r$> con <- ellipse_connect(env = "PROD")
ℹ Environnement: PROD
ℹ Database: datawarehouse
INFO [2024-06-25 17:49:18] [get_aws_credentials] successful connection to aws
ℹ Pour déconnecter: tube::ellipse_disconnect(objet_de_connexion)
```

### Découvrir les données

La première étape de toute analyse est de rencenser les données à notre disposition. C'est le rôle de la fonction `ellipse_discover()`. Elle prend minimalement en paramètre l'objet de connexion obtenu à l'étape précédente :

```r
[ins] r$> con <- ellipse_connect(env = "DEV")
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
            dplyr::mutate(
              date_heure = lubridate::as_datetime(extraction_datetime,
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

Pour la documentation conceptuelle de la plateforme de données du CAPP, voir le répertoire [doc](https://github.com/ellipse-science/tube-doc/tree/main)
