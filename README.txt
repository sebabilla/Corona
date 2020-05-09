Animated data vizualization about the spread of Covid19 in Hokkaido

by Sébastien Abilla

The files are to be executed in order (1 to 7), before executing 9 to generate the images.
1. Import data and make a first non-specific cleaning
2. Generates global variables and datasets used by several functions
3. Function that generates a histogram age/sex/job of the patients
4. Function that generates a map of the residence place of the patients
5. Function that generates a histogram of the number of new cases discovered and the trend of the infection.
6. Function to plot the casualities
7. Run the code makes 2 glm modelisations, test them and keep best fits for print: P(symptom/demographics), P(infected/demogrphics). Train and test set cut is random, if a city is present only in the test set, that generates error and the code as to be run again.
9. Generates as many images as the number of days past since the first case. (To be gather in a movie with an external soft, I use Pitivi) 

99. Code lines cemetery (that may be usefull)

Data about patients from the opendata page of Hokkaido Government :
https://www.harp.lg.jp/opendata/dataset/1369.html




