
#include <RcppArmadillo.h>

double rmsd(
  const arma::vec &x
){

  return std::sqrt(
    arma::mean(
      arma::square(
        x.elem( arma::find_finite(x) )
      )
    )
  );

}

double euc_dist(
  const arma::vec &x1,
  const arma::vec &x2
){

  return std::sqrt(
    arma::sum(arma::square(x2 - x1))
  );

}

// [[Rcpp::export]]
arma::vec ac_coord_dists(
  arma::mat coords1,
  arma::mat coords2
){

  // Check row dimensions
  if(coords1.n_rows != coords2.n_rows){
    Rf_error("Dimensions of coordinates do not match");
  }

  // Expand coords to match maximum dimensions
  int dims = arma::max( arma::uvec{ coords1.n_cols, coords2.n_cols } );
  coords1.resize(coords1.n_rows, dims);
  coords2.resize(coords2.n_rows, dims);

  // Calculate coordinate distances
  arma::vec dists(coords1.n_rows);
  double dist;
  for(arma::uword i=0; i<coords1.n_rows; i++){
    dist = 0.0;
    for(arma::uword j=0; j<coords1.n_cols; j++){
      dist += std::pow(coords1(i,j) - coords2(i,j), 2);
    }
    dists(i) = std::sqrt(dist);
  }

  // Return distances
  return dists;

}

// Get indices of rows containing NaN somewhere
arma::uvec na_row_indices(
  const arma::mat &X
){

  arma::uvec na_rows = arma::find_nonfinite( X.col(0) );
  return na_rows;

}

// Return a subset of matrix rows, non-matching elements, represented by
// -1 in the subset vector are left as NaN rows.
arma::mat subset_rows(
  const arma::mat &matrix,
  const arma::ivec &subset
){

  // Create matrix of results
  arma::mat submatrix( subset.n_elem, matrix.n_cols );
  submatrix.fill( arma::datum::nan );

  // Cycle through rows
  for(arma::uword i=0; i<subset.n_elem; i++){

    int index = subset(i);
    if(index < 0) continue; // -1 represents no match
    submatrix.row(i) = matrix.row(index);

  }

  // Return the matrix
  return submatrix;

}


// Check if openmp is used to run code in parallel
// [[Rcpp::export]]
bool parallel_mode(){

  #if defined(_OPENMP)
    return true;
  #else
    return false;
  #endif

}


