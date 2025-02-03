/*Region of Interest (ROI)*/
Map.addLayer(Bandung, {}, 'Kota Bandung');
Map.centerObject(Bandung);

/*Filter Date*/
var startDate = ee.Date('2023-01-01'); 
var endDate = ee.Date('2023-02-01'); 

/*Load Dataset*/
var CO = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_CO')
  .filterBounds(Bandung)
  .filterDate(startDate, endDate)
  .select('CO_column_number_density');

//Compute mean pixel value
var CO_Mean = CO.mean()
  .clip(Bandung);

/*Unit Convertion*/
//Set molar mass CO (g/mol)
var molarMassCO = 28.01;

//Volume column (0-10000m = Lower Troposphere)
var volCol = 1000;

//Conversion from mol/m^2 to µg/m³
var conversionFactor = molarMassCO * 1e6 / volCol;
var CO_MeanMicrograms = CO_Mean.multiply(conversionFactor);

//Parameter Visual
var visParam = {
  min: 645.5495223633487,
  max: 730.9098271891498, // Or you can set based on your region
  palette: ['black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red']
};

Map.addLayer(CO_MeanMicrograms, visParam, 'Januari CO Concentration (µg/m³)');

/*Export*/
Export.image.toDrive({
  image: CO_MeanMicrograms,
  description: 'Januari_Konsentrasi_CO',
  scale: 1113.2,
  folder: 'Sentinel5P',
  region: Bandung,
  fileFormat: 'GeoTIFF'
  });