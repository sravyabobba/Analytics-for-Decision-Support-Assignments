set numbers;

param P {i in numbers};
param C {i in numbers};
param E {i in numbers};

var X {i in numbers} >= 0, <= 75;

maximize next_year_price: sum {i in numbers} (150 - X[i])*E[i];

subject to
Money_Required_Today: sum {i in numbers} (X[i]*(0.69*C[i] + 0.3*P[i])) = 10000;


