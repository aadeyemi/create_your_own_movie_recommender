#include <fstream>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <cstdlib>

using namespace std;

vector<string> split(string str, char delimiter) {
  vector<string> internal;
  stringstream ss(str);
  string tok;

  while(getline(ss, tok, delimiter)) {
    internal.push_back(tok);
  }

  return internal;
}

int main(int, char **) {
    ifstream file ( "H:/Work/MovieRecommender/data/ml-latest-small/ratings.csv" );
    string value;

    if(file.good()) getline (file, value);

    vector<int>    userId  (105340,0);
    vector<int>    movieId (105340,0);
    vector<double> rating  (105340,0);

    // Read File
    int k = 0;
    int nUserId = -1;
    int nMovieId = -1;
    while ( !file.eof() ) {
        getline ( file, value);
        vector<string> sep = split(value, ',');

        if (sep.size() > 3) {
            userId[k]  = atoi( sep[0].c_str() );
            movieId[k] = atoi( sep[1].c_str() );
            rating[k]  = atof( sep[2].c_str() );

            if (userId[k]  > nUserId)  nUserId  = userId[k];
            if (movieId[k] > nMovieId) nMovieId = movieId[k];

            k = k+1;
            cout << "line: " << k << endl;
        }
    }
    file.close();

    // File Content to Matrix
    typedef vector <double> Rowd;
    typedef vector <int> Rowi;
    typedef vector <Rowd> Matrixd;
    typedef vector <Rowi> Matrixi;

    Matrixd ratingsMatrix (nMovieId,Rowd(nUserId,0));
    Matrixi ratedMatrix   (nMovieId,Rowi(nUserId,0));

    for (int i=0; i<k; i++) {
        ratingsMatrix[movieId[i]-1][userId[i]-1] = rating[i];
        ratedMatrix  [movieId[i]-1][userId[i]-1] = 1;
    }


    // Matrix to Output
    ofstream rmFile ( "H:/Work/MovieRecommender/ml-latest-small_ratingsMatrix.csv" );
    ofstream rFile  ( "H:/Work/MovieRecommender/ml-latest-small_ratedMatrix.csv" );
    for (int i=0; i<nMovieId; i++) {
        for (int j=0; j<nUserId-1; j++) {
            rmFile << ratingsMatrix[i][j] << ","; // put comma at end of every value on row except last one
            rFile  << ratedMatrix[i][j]   << ","; // put comma at end of every value on row except last one
        }
        rmFile << ratingsMatrix[i][nUserId-1] << endl; // do not put comma at end of last value on row
        rFile  << ratedMatrix[i][nUserId-1]   << endl; // do not put comma at end of last value on row
        cout << "Row: " << i << endl;
    }

    rmFile.close();
    rFile.close();

    return 0;
}
