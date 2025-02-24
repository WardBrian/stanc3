data {
  int d_int;
  matrix[d_int, d_int] d_matrix;
  array[d_int] matrix[d_int, d_int] d_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] d_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] d_matrix_array_3d;


}

parameters {
  matrix[d_int, d_int] p_matrix;
  array[d_int] matrix[d_int, d_int] p_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] p_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] p_matrix_array_3d;


}

transformed parameters {
  matrix[d_int, d_int] transformed_param_matrix;
  array[d_int] matrix[d_int, d_int] transformed_param_matrix_array_1d;
  array[d_int, 2] matrix[d_int, d_int] transformed_param_matrix_array_2d;
  array[d_int, 2, 3] matrix[d_int, d_int] transformed_param_matrix_array_3d;

  transformed_param_matrix = stochastic_row_jacobian(d_matrix);
  transformed_param_matrix = stochastic_row_jacobian(p_matrix);
  transformed_param_matrix_array_1d = stochastic_row_jacobian(d_matrix_array_1d);
  transformed_param_matrix_array_1d = stochastic_row_jacobian(p_matrix_array_1d);
  transformed_param_matrix_array_2d = stochastic_row_jacobian(d_matrix_array_2d);
  transformed_param_matrix_array_2d = stochastic_row_jacobian(p_matrix_array_2d);
  transformed_param_matrix_array_3d = stochastic_row_jacobian(d_matrix_array_3d);
  transformed_param_matrix_array_3d = stochastic_row_jacobian(p_matrix_array_3d);
}

