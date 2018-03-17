'''
This python script converts point csv data into a point shapefile.
'''

# import module
import csv
import arcpy
import time

start_time = time.time()

# set path, work environment; set overwrite as true
path = "U:\\Desktop\\nyct2000_16c"
arcpy.env.overwriteOutput = True
arcpy.env.workspace = path

# create empty shapefile, be of a point geometry type
out_path = path
out_name = 'By vehicle_Stateplane_dwell_without_charge.shp' # create output 
NYC_shapefile = "nyct2000.shp"
spRef = arcpy.Describe(NYC_shapefile).spatialReference # same as NYC TAZ file
#arcpy.env.outputCoordinateSystem = spRef
arcpy.CreateFeatureclass_management(out_path, out_name, 'POINT', '', '', '', spRef)

# empty shapefile must already exist, be of a point geometry type 
# import GPS data
cursor = arcpy.InsertCursor(path + '\\' + out_name) # use insert cursor

with open('By vehicle_Stateplane_dwell_without_charge.csv', 'rb') as f:
    reader = csv.DictReader(f)
    for row in reader:
        # Create the feature
        feature = cursor.newRow()

        # Add the point geometry to the feature
        vertex = arcpy.CreateObject("Point")
        vertex.X = row['dropoff_longitude']
        vertex.Y = row['dropoff_latitude']
        feature.shape = vertex

        # write to shapefile
        cursor.insertRow(feature)

# clean up
del cursor

print("--- %s minutes ---" % ((time.time() - start_time)/60))
