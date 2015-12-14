# Set the system env variable
Sys.setenv(SPARK_HOME = "C:/Apache/spark-1.5.2-bin-hadoop2.6")

# Set the library path
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))

# load the spark library
library(SparkR)

# Create a spark context and a SQL context
sc <- sparkR.init(master = "local")
sqlContext <- sparkRSQL.init(sc)


# create a sparkR DataFrame
DF <- createDataFrame(sqlContext, data)
head(DF)
