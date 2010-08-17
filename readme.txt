This zip contains map-metadata for the optional sturmonanny recon map generator.


Mapbase-folder configuration
===========================================================================


Inclusion in sturmonanny:  
===========================================================================

Set up a post-processor (positive number) in your sturmonanny instance 
configuration (e.g. default.conf) under Configuration/fbdj/DCG/addons.

There are two possible classes: 
de.immaterialien.qlmap.MissionFilter  
  - creates [missionName].mis.JPG files in the missions folder
de.immaterialien.qlmap.HtmlMissionFilter 
  - creates not only the [missionName].mis.JPG files but also some light HTML glue like 
      [missionName].mis.JPG.recon.html (html including a "timeline" iframe with links to 
        all existing mission recons)
      recon.latest.html (plain iframe pointing at the latest [missionName].mis.JPG.recon.html)
      recon.list.html (contains links to all *.mis.JPG.recon.html, target of the "timeline" iframe)  

The addonArguments section should contain a link to the mapbase folder.

Example configuration (note the underscores replacing the dots):
<fbdj>
  [...]
  <DCG>
    [...]
    <addons>
      de_immaterialien_qlmap_HtmlMissionFilter = 10
    </addons>
    <addonArguments>
      de_immaterialien_qlmap_HtmlMissionFilter = "C:/serverstuff/mapbase"  # or C:/serverstuff/mapbase/out.conf
    </addonArguments>
    [...]
  </DCG>
  [...]
</fbdj>
    

Note: the map code itself is included in the latest sturmonanny versions, this file is the mapbase container. 

Either add this jar to the sturmonanny classpath in the start script (start.bat) or unpack it and configure the 
path as described above, for easy customization/extension of the map list.  

 - usrusr