# esriUrl_parseUrl

    Code
      esriUrl_parseUrl(
        "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3")
    Output
      $url
      [1] "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer/3"
      
      $scheme
      [1] "https://"
      
      $host
      [1] "sampleserver6.arcgisonline.com"
      
      $instance
      [1] "arcgis"
      
      $restIndicator
      [1] "rest/services"
      
      $folderPath
      [1] ""
      
      $serviceName
      [1] "Census"
      
      $serviceType
      [1] "MapServer"
      
      $featureID
      [1] 3
      

---

    Code
      esriUrl_parseUrl(
        "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer")
    Output
      $url
      [1] "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
      
      $scheme
      [1] "https://"
      
      $host
      [1] "sampleserver6.arcgisonline.com"
      
      $instance
      [1] "arcgis"
      
      $restIndicator
      [1] "rest/services"
      
      $folderPath
      [1] ""
      
      $serviceName
      [1] "Census"
      
      $serviceType
      [1] "MapServer"
      
      $featureID
      integer(0)
      

---

    Code
      esriUrl_parseUrl("https://sampleserver6.arcgisonline.com/ArcGIS/rest/services")
    Output
      $url
      [1] "https://sampleserver6.arcgisonline.com/ArcGIS/rest/services"
      
      $scheme
      [1] "https://"
      
      $host
      [1] "sampleserver6.arcgisonline.com"
      
      $instance
      [1] "ArcGIS"
      
      $restIndicator
      [1] "rest/services"
      
      $folderPath
      [1] ""
      
      $serviceName
      [1] ""
      
      $serviceType
      [1] ""
      
      $featureID
      integer(0)
      

---

    Code
      esriUrl_parseUrl(
        "https://services.arcgis.com/V6ZHFr6zdgNZuVG0/arcgis/rest/services/Landscape_Trees/FeatureServer/0/")
    Output
      $url
      [1] "https://services.arcgis.com/V6ZHFr6zdgNZuVG0/arcgis/rest/services/Landscape_Trees/FeatureServer/0/"
      
      $scheme
      [1] "https://"
      
      $host
      [1] "services.arcgis.com"
      
      $instance
      [1] "V6ZHFr6zdgNZuVG0/arcgis"
      
      $restIndicator
      [1] "rest/services"
      
      $folderPath
      [1] ""
      
      $serviceName
      [1] "Landscape_Trees"
      
      $serviceType
      [1] "FeatureServer"
      
      $featureID
      [1] 0
      

