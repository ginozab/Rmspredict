# `mspredictr`

`mspredictr` is an `R package` that helps ease the process of running data mining and supervised machine learning to make predictions on target data.

`mspredictr` makes use of the `caret` package in the `R` language for statistical computation to run the machine learning and data mining techniques.

## Functions available to call

There are three different categories of functions provided by the `mspredictr` package. These categories include prediction functions, benchmarking functions, and visualization functions.

| Function Type | Function Name    |
|---------------|------------------|
| Prediction    | cat_prediction   |
| Prediction    | num_predict      |
| Prediction    | cat_majority     |
| Prediction    | reg_average_pred |
| Benchmarking  | benchmark        |
| Benchmarking  | pred_bench       |
| Benchmarking  | pred_frame       |
| Visualization |                  |
| Visualization |                  |

### Prediction Functions

There are two main prediction functions made available in the `mspredictr` package along with two base prediction functions. The first prediction function is the `cat_prediction` function which helps facilitate making predictions on categorical data. The second function is the `num_predict` function which helps make predictions on numerical data types.

These two functions take require three parameters. The first parameter is the dataset from which training data and target prediction data. The second is the machine learning algorithm that should be used to make the predictive model for making predictions. The final parameter is to specify on which data the function should be making predictions.

The two base functions are targeted for two different target predictor data types. The first base prediction function is targeted for categorical data predictions, and predicts the majority of a categorical occurrence in the training data. This function is the `cat_majority` function. The second base function is target for numerical base predictions, and this function predicts the average of the numerical occurrences in training data. This function is the `reg_average_pred` function.

### Benchmarking Functions

The main benchmarking function is the `benchmark` function which can help automate run however many desired trials of predictions using a list of algorithms and a list of target prediction data.
This `benchmark` function makes use of the next two functions available in the package, the `pred_bench` function and the `pred_frame` function. The `pred_bench` function automates a desired number of trials using a list of methods to predict on one target data. The `pred_frame` function is a frame for all possible prediction configurations. The `pred_frame` function can run categorical and numerical predictions using any type of method to predict any desired target data.

### Visualization Functions

## Data Available for Use
## Usage Example

[![asciicast](https://asciinema.org/a/41901.png)](https://asciinema.org/a/41901)
