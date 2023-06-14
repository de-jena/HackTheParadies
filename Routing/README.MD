Dieser Artikel gibt einen Überblick über die Möglichkeiten zum Routen im GIS. Dafür stehen zwei verschiedene Routing-Dienste bereit, Valhalla und Openrouteservice. Diese stehen für innerhalb der Stadtverwaltung zur Nutzung zur Verfügung. Im Folgenden ist die Funktionsweise beider Dienste genauer erklärt. 

==Valhalla==
Der Dienst ist erreichbar unter:

*<code>[https://routing-valhalla.jena.de/ https://routing-valhalla.jena.de]</code>

Nähere Infos zur Software Valhalla sind unter https://valhalla.github.io/valhalla/ zu finden.

===Verwendung in externen Anwendungen===
'''QGIS'''

Eine Verwendung des Routing-Dienstes innerhalb von QGIS ist über ist über das Plugin "Valhalla" möglich.

'''Python'''

Zur Verwendung in Python gibt es eine entsprechende Python-Bibliothek '''pyvalhalla'''. Nähere Informationen unter: https://pypi.org/project/pyvalhalla/.

'''R'''

Die Implementierung in R ist unter anderem möglich über das Paket '''valhallr'''. Nähere Informationen unter: https://cran.r-project.org/web/packages/valhallr/vignettes/valhallr.html. 

Für die Verwendung innerhalb der Stadtverwaltung zunächst die Programme R bzw. RStudio über den IT-Service  zuweisen lassen.

===Direkte Verwendung des Valhalla-Dienstes===
Der Valhalla-Dienst bietet eine Reihe von Endpunkten für verschiedene Zwecke. Standardmäßig sind diese verfügbar unter:

*<code>https://routing-valhalla.jena.de/route</code>  (Route)
*<code>https://routing-valhalla.jena.de/optimized_route</code>  (Optimale Route)
*<code>https://routing-valhalla.jena.de/sources_to_targets</code> (Distanzmatrix)
*<code>https://routing-valhalla.jena.de/isochrone</code> (Isochrone/Isodistanzen)
*<code>https://routing-valhalla.jena.de/locate</code>(Verortung)

Im den folgenden Abschnitten ist eine direkt Ansprache des Dienstes über entsprechende Konsolen-Befehle näher erläutert.

====Profile====
Das Profil bestimmt das Verkehrsmittel, welches zum Routen verwendet wird. Die für das Routing verwendbaren Profile sind:
{| class="wikitable"
!Profil
!Bezeichnung
|-
|auto
|Auto
|-
|bicycle
|Fahrrad
|-
|bus
|Bus
|-
|truck
|LKW
|-
|taxi
|Taxi
|-
|motor_scooter
|Motorroller und Moped
|-
|motorcycle (beta)
|Motorrad
|-
|pedestrian
|Fußgänger
|}
Das Profil wird in der URL des jeweiligen Aufrufs als Kosten-Parameter mit übergeben, z.B.: <code>"costing":"auto"</code>. Genaueres in den Beispielen.

====Beispiele====
Hier sind einige Beispiele aufgelistet, mit denen sich entsprechende Befehle absetzen lassen, welche das Ergebnis der Berechnungen als json zurückliefert. 

Eine weitaus detailiertere Beschreibung bietet die offizielle API-Dokumentation von Valhalla (in englischer Sprache) unter https://valhalla.github.io/valhalla/api/.

'''Hinweis: In Windows die Befehle in der Eingabeaufforderung ausführen, nicht in der PowerShell!'''
=====Route=====
'''Direkt-Aufruf im Browser'''
 [https://routing-valhalla.jena.de/route?json={&#x22;costing&#x22;:&#x22;auto&#x22;,&#x22;costing_options&#x22;:{&#x22;auto&#x22;:{&#x22;top_speed&#x22;:130,&#x22;shortest&#x22;:false}},&#x22;locations&#x22;:&#x5B;{&#x22;lon&#x22;:11.5923689,&#x22;lat&#x22;:50.9321657},{&#x22;lon&#x22;:11.5889912,&#x22;lat&#x22;:50.9145299}&#x5D;,&#x22;directions_options&#x22;:{&#x22;units&#x22;:&#x22;kilometers&#x22;},&#x22;id&#x22;:&#x22;valhalla_directions&#x22;} <nowiki>https://routing-valhalla.jena.de/route?json={</nowiki><nowiki>"costing":"auto","costing_options":{"auto":{"top_speed":130,"shortest":false}},"locations":[{"lon":11.5923689,"lat":50.9321657},{"lon":11.5889912,"lat":50.9145299}],"directions_options":{"units":"kilometers"},"id":"valhalla_directions"}</nowiki>]
'''Konsole Linux'''
 curl <nowiki>https://routing-valhalla.jena.de/route</nowiki><nowiki> --data '{"costing":"auto","costing_options":{"auto":{"top_speed":130,"shortest":false}},"locations":[{"lon":11.5923689,"lat":50.9321657},{"lon":11.5889912,"lat":50.9145299}],"directions_options":{"units":"kilometers"},"id":"valhalla_directions"}'</nowiki>
'''Konsole Windows'''
 curl <nowiki>https://routing-valhalla.jena.de/route</nowiki><nowiki> --data "{"""costing""":"""auto""","""costing_options""":{"""auto""":{"""top_speed""":130,"""shortest""":false}},"""locations""":[{"""lon""":11.5923689,"""lat""":50.9321657},{"""lon""":11.5889912,"""lat""":50.9145299}],"""directions_options""":{"""units""":"""kilometers"""},"""id""":"""valhalla_directions"""}"</nowiki>
'''WICHTIGER HINWEIS'''

Die Geometrie der berechneten Route wird kodiert ausgegeben und enthält zunächst keine direkt lesbaren Koordinaten/Linien! Bei Google gibt es [https://developers.google.com/maps/documentation/utilities/polylinealgorithm?hl=de eine genauere Beschreibung dazu]. Um die Geometrie weiterzuverwenden, muss diese zunächst dekodiert werden. 

Nähere Informationen mit konkreten Code-Beispielen sind hier zu finden: https://valhalla.github.io/valhalla/decoding/. 

Das R-Paket valhallr hat dafür eine eigene Funktion: https://rdrr.io/cran/valhallr/man/decode.html.

=====Isochrones=====
'''Direkt-Aufruf im Browser'''
 [https://routing-valhalla.jena.de/isochrone?json={&#x22;costing&#x22;:&#x22;auto&#x22;,&#x22;costing_options&#x22;:{&#x22;auto&#x22;:{&#x22;top_speed&#x22;:130,&#x22;shortest&#x22;:false}},&#x22;locations&#x22;:&#x5B;{&#x22;lon&#x22;:11.5915772,&#x22;lat&#x22;:50.930815800000005}&#x5D;,&#x22;directions_options&#x22;:{&#x22;units&#x22;:&#x22;kilometers&#x22;},&#x22;contours&#x22;:&#x5B;{&#x22;time&#x22;:10},{&#x22;time&#x22;:5}&#x5D;,&#x22;id&#x22;:&#x22;valhalla_isochrone&#x22;} <nowiki>https://routing-valhalla.jena.de/isochrone?json={</nowiki><nowiki>"costing":"auto","costing_options":{"auto":{"top_speed":130,"shortest":false}},"locations":[{"lon":11.5915772,"lat":50.930815800000005}],"directions_options":{"units":"kilometers"},"contours":[{"time":10},{"time":5}],"id":"valhalla_isochrone"}</nowiki>]
'''Konsole Linux'''
 curl <nowiki>https://routing-valhalla.jena.de/isochrone</nowiki><nowiki> --data '{"costing":"auto","costing_options":{"auto":{"top_speed":130,"shortest":false}},"locations":[{"lon":11.5915772,"lat":50.930815800000005}],"directions_options":{"units":"kilometers"},"contours":[{"time":10},{"time":5}],"id":"valhalla_isochrone"}'</nowiki>
'''Konsole Windows'''
 curl <nowiki>https://routing-valhalla.jena.de/isochrone</nowiki><nowiki> --data "{"""costing""":"""auto""","""costing_options""":{"""auto""":{"""top_speed""":130,"""shortest""":false}},"""locations""":[{"""lon""":11.5915772,"""lat""":50.930815800000005}],"""directions_options""":{"""units""":"""kilometers"""},"""contours""":[{"""time""":10},{"""time""":5}],"""id""":"""valhalla_isochrone"""}"</nowiki>

=====Matrix=====
'''Direkt-Aufruf im Browser'''
 https://routing-valhalla.jena.de/sources_to_targets?json={&#x22;sources&#x22;:&#x5B;{&#x22;lat&#x22;:50.9308604,&#x22;lon&#x22;:11.5894326},{&#x22;lat&#x22;:50.9515362,&#x22;lon&#x22;:11.6085817},{&#x22;lat&#x22;:50.9351937,&#x22;lon&#x22;:11.5600964},{&#x22;lat&#x22;:50.8815372,&#x22;lon&#x22;:11.5902583}&#x5D;,&#x22;targets&#x22;:&#x5B;{&#x22;lat&#x22;:50.9308604,&#x22;lon&#x22;:11.5894326},{&#x22;lat&#x22;:50.9515362,&#x22;lon&#x22;:11.6085817},{&#x22;lat&#x22;:50.9351937,&#x22;lon&#x22;:11.5600964},{&#x22;lat&#x22;:50.8815372,&#x22;lon&#x22;:11.5902583}&#x5D;,&#x22;costing&#x22;:&#x22;auto&#x22;,&#x22;id&#x22;:&#x22;valhalla_matrix&#x22;}
'''Konsole Linux'''
 curl <nowiki>https://routing-valhalla.jena.de/sources_to_targets</nowiki> --data '{"sources":[{"lat":50.9308604,"lon":11.5894326},{"lat":50.9515362,"lon":11.6085817},{"lat":50.9351937,"lon":11.5600964},{"lat":50.8815372,"lon":11.5902583}],"targets":[{"lat":50.9308604,"lon":11.5894326},{"lat":50.9515362,"lon":11.6085817},{"lat":50.9351937,"lon":11.5600964},{"lat":50.8815372,"lon":11.5902583}],"costing":"auto","id":"valhalla_matrix"}'
'''Konsole Windows'''
 curl <nowiki>https://routing-valhalla.jena.de/sources_to_targets</nowiki> --data "{"""sources""":[{"""lat""":50.9308604,"""lon""":11.5894326},{"""lat""":50.9515362,"""lon""":11.6085817},{"""lat""":50.9351937,"""lon""":11.5600964},{"""lat""":50.8815372,"""lon""":11.5902583}],"""targets""":[{"""lat""":50.9308604,"""lon""":11.5894326},{"""lat""":50.9515362,"""lon""":11.6085817},{"""lat""":50.9351937,"""lon""":11.5600964},{"""lat""":50.8815372,"""lon""":11.5902583}],"""costing""":"""auto""","""id""":"""valhalla_matrix"""}"
==Openrouteservice==
Der Dienst ist erreichbar unter:

*<code>https://routing-ors.jena.de/ors/</code>

Nähere Infos zur Software Openrouteservice sind unter https://openrouteservice.org/ zu finden.

===Verwendung in externen Anwendungen===
'''QGIS'''

Eine Verwendung des Routing-Dienstes über das Plugin "ORS Tools " gibt der Abschnitt [[QGIS#Routing|Routing auf der QGIS-Wiki-Seite]].

'''Python'''

Zur Verwendung in Python gibt es eine entsprechende Python-Bibliothek '''openrouteservice-py'''. Nähere Informationen unter: https://openrouteservice-py.readthedocs.io/en/latest/.

'''R'''

Die Implementierung in R ist unter anderem möglich über das Paket '''openrouteservice-r'''. Nähere Informationen unter: https://giscience.github.io/openrouteservice-r/. 

Für die Verwendung innerhalb der Stadtverwaltung zunächst die Programme R bzw. RStudio über den IT-Service  zuweisen lassen.

===Verwendung des ORS-Dienstes===
Der Openrouteservice-Dienst bietet eine Reihe von Endpunkten für verschiedene Zwecke. Standardmäßig sind diese verfügbar unter:

*<code>https://routing-ors.jena.de/ors/v2/directions</code> (Strecken)
*<code>https://routing-ors.jena.de/ors/v2/isochrones</code> (Isochrone/Isodistanzen)
*<code>https://routing-ors.jena.de/ors/v2/matrix</code> (Distanzmatrizen)

Im den folgenden Abschnitten ist eine direkt Ansprache des Dienstes über entsprechende Konsolen-Befehle näher erläutert.

====Profile====
Das Profil bestimmt das Verkehrsmittel, welches zum Routen verwendet wird. Die für das Routing verwendbaren Profile sind:
{| class="wikitable"
!Profil
!Bezeichnung
|-
|driving-car
|Auto
|-
|driving-hgv
|LKW
|-
|cycling-regular
|Fahrrad
|-
|cycling-road
|City-/Straßen-Fahrrad
|-
|cycling-mountain
|Mountainbike
|-
|cycling-electric
|E-Bike
|-
|foot-walking
|Laufen
|-
|foot-hiking
|Wandern
|-
|wheelchair
|Rollstuhl
|}
Das Profil wird der URL des jeweiligen Aufrufs mit übergeben. Z.B:

*<code>https://routing-ors.jena.de/ors/v2/directions/driving-car</code>
*<code>https://routing-ors.jena.de/ors/v2/isochrones/cycling-regular</code>
*<code>https://routing-ors.jena.de/ors/v2/matrix/foot-walking</code>

====Beispiele====
Hier sind einige Beispiele aufgelistet, mit denen sich entsprechende Befehle absetzen lassen, welche das Ergebnis der Berechnungen als geojson zurückliefert. 

Eine weitaus detailiertere Beschreibung und auch die Möglichkeit, sich die Befehle "zusammenzubauen" bietet die offizielle API-Dokumentation des Openrouteservice (in englischer Sprache) unter https://openrouteservice.org/dev/#/api-docs/.



'''Hinweis: In Windows die Befehle in der Eingabeaufforderung ausführen, nicht in der PowerShell!'''
=====Directions=====
'''GET-Request, funktioniert nur mit zwei Koordinatenpaaren (Start und Ende)'''

'''Linux/Windows'''
 curl --include --header "Content-Type: application/json; charset=utf-8" --header "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" "<nowiki>https://routing-ors.jena.de/ors/v2/directions/driving-car?start=11.5894326,50.930857&end=11.5902585,50.8815338</nowiki>" > "/folder/direction.geojson"


'''POST-Request für mehrere Koordinatenpaare'''

'''Linux'''
 curl -X POST "<nowiki>https://routing-ors.jena.de/ors/v2/directions/driving-car/geojson</nowiki>" -H "Content-Type: application/json; charset=utf-8" -H "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" -d '{"coordinates":[[11.6003647,50.9435852],[11.5894326,50.930857],[11.5902585,50.8815338]],"elevation":"true"}' > "/folder/direction.geojson"
'''Windows'''
 curl -X POST "<nowiki>https://routing-ors.jena.de/ors/v2/directions/driving-car/geojson</nowiki>" -H "Content-Type: application/json; charset=utf-8" -H "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" -d "{"""coordinates""":[[11.6003647,50.9435852],[11.5894326,50.930857],[11.5902585,50.8815338]],"""elevation""":"""true"""}" > "/folder/direction.geojson"

=====Isochrones=====
Mögliche Parameter für <code>location_type</code>: <code>start</code>, <code>destination</code>

Mögliche Parameter für <code>range_type</code>: <code>time</code>, <code>distance</code>

'''Linux'''
 curl -X POST "<nowiki>https://routing-ors.jena.de/ors/v2/isochrones/driving-car</nowiki>" -H "Content-Type: application/json; charset=utf-8" -H "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" -d '{"locations":<nowiki>[[11.5894326,50.930857]]</nowiki>,"range":[300,600,900],"intersections":"true","location_type":"destination","range_type":"time"}' > "/folder/isochrones.geojson"
'''Windows'''
 curl -X POST "<nowiki>https://routing-ors.jena.de/ors/v2/isochrones/driving-car</nowiki>" -H "Content-Type: application/json; charset=utf-8" -H "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" -d "{"""locations""":<nowiki>[[11.5894326,50.930857]]</nowiki>,"""range""":[300,600,900],"""intersections""":"""true""","""location_type""":"""destination""","""range_type""":"""time"""}" > "/folder/isochrones.geojson"

=====Matrix=====
'''Linux'''
 curl -X POST "<nowiki>https://routing-ors.jena.de/ors/v2/matrix/driving-car</nowiki>" -H "Content-Type: application/json; charset=utf-8" -H "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" -d '{"locations":[[11.6003647,50.9435852],[11.5894326,50.930857],[11.5902585,50.8815338]]}' > "/folder/matrix.geojson"
'''Windows'''
 curl -X POST "<nowiki>https://routing-ors.jena.de/ors/v2/matrix/driving-car</nowiki>" -H "Content-Type: application/json; charset=utf-8" -H "Accept: application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8" -d "{"""locations""":[[11.6003647,50.9435852],[11.5894326,50.930857],[11.5902585,50.8815338]]}" > "/folder/matrix.geojson"
   [[Kategorie:IT-Service]]
  
   
   [[Kategorie:GIS]]
