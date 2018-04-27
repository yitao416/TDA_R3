# TDA_R1

Using R to generate input files and process the results.


Download the GUDHI utils

https://gforge.inria.fr/frs/download.php/file/37366/2018-01-31-09-25-53_GUDHI_2.1.0_WIN64_UTILS.zip


Usage: rips_distance_matrix_persistence.exe [options] input-file

Allowed options:
  -h [ --help ]                       produce help message
  -o [ --output-file ] arg            Name of file in which the persistence diagram is written.
                                      Default print in std::cout
  -r [ --max-edge-length ] arg (=inf) Maximal length of an edge for the Rips complex construction.
  -d [ --cpx-dimension ] arg (=1)     Maximal dimension of the Rips complex we want to compute.
  -p [ --field-charac ] arg (=11)     Characteristic p of the coefficient field Z/pZ for computing
                                      homology.
  -m [ --min-persistence ] arg        Minimal lifetime of homology feature to be recorded. Default
                                      is 0. Enter a negative value to see zero length intervals


For example:
$rips_distance_matrix_persistence.exe -o 120B3seed72.txt -r 17 -d 4 -p 2 ./data/distance_matrix/120B3seed72.csv
