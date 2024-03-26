# tube 🚰

![Le tube](tube.png)
(Image par [Dall-E 3](https://openai.com/dall-e-3))

Interface R d'accès à la plateforme de données _Ellipse_.

## Pré-requis

Pour accéder aux données de la plateforme, il faut configurer les clés d'accès AWS. Pour obtenir ces informations, contacter Patrick Poncet (@patoscope) sur Slack.

Ensuite, il faut les ajouter au fichier `~/.Renviron` qui est chargé au démarrage de la session R :

```R
AWS_ACCESS_KEY_ID=<identifiant de clé>
AWS_SECRET_ACCESS_KEY=<clé d'accès secrète>
```

## Interface de haut hiveau

`tube` comporte une interface de haut niveau qui permet d'interroger la plateforme à l'aide de fonctions d'analyse de données fournies par le `tidyverse`.

Ces fonctions sont bâties à même une architecture technique décrite dans la section [Interface technique](#interface-technique).

Pour faciliter la découverte des fonctionnalités, les noms de fonction commencent par `ellipse_`. Lorsque `tube` est chargé avec `library(tube)`, taper `ell<TAB>` permet de voir rapidement les fonctions disponibles.

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
[ins] r$> ellipse_partitions(con, "a-parliament-debates")
INFO [2024-03-24 21:22:54] [tube::list_glue_tables] listing tables from the datawarehouse
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
[[1]]
# A tibble: 2 × 1
  institution_id
  <chr>
1 CACOMMONS
2 QCASSNAT

[[2]]
# A tibble: 43 × 1
   event_date
   <date>
 1 2007-01-29
 2 2023-11-29
 3 2023-11-30
 4 2023-12-01
 5 2023-12-05
 6 2023-12-06
 7 2023-12-07
 8 2023-12-08
 9 2023-12-12
10 2023-12-13
# ℹ 33 more rows
# ℹ Use `print(n = ...)` to see more rows
```

Une liste est retournée, dont chaque élément correspond aux valeurs possibles pour une des variables partitionnées de la table. Ces valeurs peuvent nous guider dans nos requêtes subséquentes. À l'usage, pour obtenir une partie des données, on remarquera que l'utilisation d'un filtre sur des variables partionnées sera beaucoup plus rapide que sur des variables non-partitionnées. Il est donc recommandé d'utiliser les filtres de variables partitionnées en premier puis ceux sur les variables non-partionnées pour raffiner.

Si une des variables partitionnées comporte beaucoup de valeurs (c'est souvent le cas des dates), on peut obtenir un résumé plutôt qu'une liste exhaustive en mettant un maximum de valeurs avec le paramètre `max_n` :

```r
[ins] r$> ellipse_partitions(con, "a-parliament-debates", max_n=20)
INFO [2024-03-24 21:35:53] [tube::list_glue_tables] listing tables from the datawarehouse
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
INFO: (Data scanned: 0 Bytes)
[[1]]
# A tibble: 2 × 1
  institution_id
  <chr>
1 CACOMMONS
2 QCASSNAT

[[2]] # <- il y avait plus de 20 valeurs ici!
# A tibble: 1 × 3
  partition  min        max
  <chr>      <date>     <date>
1 event_date 2007-01-29 2024-03-22
```

### Interroger les données

Maintenant qu'on a une idée des données qui nous intéressent et de la façon dont elles sont partitionnées, on peut les interroger.

La fonction `ellipse_query()` nous retourne un objet qui est exploitable avec `dplyr`.

```r
[ins] r$> df_agora <- ellipse_query(con, "a-parliament-debates")
INFO: (Data scanned: 0 Bytes)
```

Combien y a-t-il d'interventions par mois aux communes et à l'assemblée nationale?

```r
[ins] r$> library(dplyr, warn.conflicts = FALSE)

[ins] r$> library(lubridate, warn.conflicts = FALSE)

[ins] r$> df_agora |>
          mutate(year = year(event_date), month = month(event_date)) |>
          count(institution_id, year, month, name = "n_interventions") |>
          collect() |>
          arrange(institution_id, year, month)
INFO: (Data scanned: 0 Bytes)
# A tibble: 10 × 4
   institution_id    year month n_interventions
   <chr>          <int64> <chr>         <int64>
 1 CACOMMONS         2007 01                245
 2 CACOMMONS         2023 12               9412
 3 CACOMMONS         2024 01               5957
 4 CACOMMONS         2024 02              50526
 5 CACOMMONS         2024 03               5275
 6 QCASSNAT          2023 11               2228
 7 QCASSNAT          2023 12              18541
 8 QCASSNAT          2024 01               2980
 9 QCASSNAT          2024 02              22556
10 QCASSNAT          2024 03              19724
```

Il faudrait plus de requêtes pour expliquer ces chiffres, bien sûr 🙂

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
