Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population
============================================================================================================

`pubBonneryBreidtCoquet2016` is a R package that contains the source
code to reproduce the simulations of ["Asymptotics for the maximum
sample likelihood estimator under informative selection from a finite
population"](http://www.e-publications.org/ims/submission/BEJ/user/submissionFile/23537?confirm=3b2ff5b3)
by Bonnery, Breidt and Coquet

Execution
---------

    devtools::install_github("DanielBonnery/pubBonneryBreidtCoquet2016")
    library(pubBonneryBreidtCoquet2016)
    demo(table1)
    demo(table2)
    demo(table3)

Output
------

### Table 1

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left"><span class="math inline"><em>θ</em></span></th>
<th align="left"><span class="math inline"><em>ξ</em></span></th>
<th align="left">Conditional to</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Asymptotic Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">4</td>
<td align="left">0.1</td>
<td align="left">NULL</td>
<td align="left">4.1</td>
<td align="left">2.39</td>
<td align="left">1.35043</td>
<td align="left">0.0181758</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">4</td>
<td align="left">0.1</td>
<td align="left">NULL</td>
<td align="left">4.01</td>
<td align="left">0.311</td>
<td align="left">1.53996</td>
<td align="left">0.0309899</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">4</td>
<td align="left">0.1</td>
<td align="left">NULL</td>
<td align="left">4.01</td>
<td align="left">0.143</td>
<td align="left">1</td>
<td align="left">0.0201916</td>
<td align="left">0.0200028</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">4</td>
<td align="left">0.1</td>
<td align="left">NULL</td>
<td align="left">NaN</td>
<td align="left">NaN</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left"><span class="math inline"><em>θ</em></span></th>
<th align="left"><span class="math inline"><em>ξ</em></span></th>
<th align="left">Conditional to</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Asymptotic Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">4</td>
<td align="left">1</td>
<td align="left">NULL</td>
<td align="left">4.86</td>
<td align="left">21.5</td>
<td align="left">15.3729</td>
<td align="left">0.031139</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">4</td>
<td align="left">1</td>
<td align="left">NULL</td>
<td align="left">4.05</td>
<td align="left">1.19</td>
<td align="left">2.13148</td>
<td align="left">0.104135</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">4</td>
<td align="left">1</td>
<td align="left">NULL</td>
<td align="left">4.02</td>
<td align="left">0.416</td>
<td align="left">1</td>
<td align="left">0.0496496</td>
<td align="left">0.0509886</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">4</td>
<td align="left">1</td>
<td align="left">NULL</td>
<td align="left">NaN</td>
<td align="left">NaN</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left"><span class="math inline"><em>θ</em></span></th>
<th align="left"><span class="math inline"><em>ξ</em></span></th>
<th align="left">Conditional to</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Asymptotic Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">4</td>
<td align="left">2</td>
<td align="left">NULL</td>
<td align="left">5.59</td>
<td align="left">39.7</td>
<td align="left">33.8249</td>
<td align="left">0.0567029</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">4</td>
<td align="left">2</td>
<td align="left">NULL</td>
<td align="left">4.06</td>
<td align="left">1.4</td>
<td align="left">1.87451</td>
<td align="left">0.139452</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">4</td>
<td align="left">2</td>
<td align="left">NULL</td>
<td align="left">4.02</td>
<td align="left">0.419</td>
<td align="left">1</td>
<td align="left">0.0757842</td>
<td align="left">0.0771277</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">4</td>
<td align="left">2</td>
<td align="left">NULL</td>
<td align="left">NaN</td>
<td align="left">NaN</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

### Table 2

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left"><span class="math inline"><em>θ</em></span></th>
<th align="left"><span class="math inline"><em>ξ</em></span></th>
<th align="left">Conditional to</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Asymptotic Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">0.1, 1.0, 1.0</td>
<td align="left">2.270, 0.788, 1.780</td>
<td align="left">355.0, -21.2, -11.2</td>
<td align="left">89.45800, 3.22033, 6.34327</td>
<td align="left">0.0354351, 0.0148147, 0.0083901</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">0.1, 1.0, 1.0</td>
<td align="left">0.508, 1.000, 1.980</td>
<td align="left">1.550, 0.147, -0.810</td>
<td align="left">1.99281, 1.90524, 1.87630</td>
<td align="left">0.0707659, 0.0354644, 0.0171822</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">0.1, 1.0, 1.0</td>
<td align="left">0.512, 0.995, 1.990</td>
<td align="left">2.330, -0.538, -0.324</td>
<td align="left">1, 1, 1</td>
<td align="left">0.03540450, 0.01858640, 0.00925542</td>
<td align="left">0.03223440, 0.01784980, 0.00885569</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">0.1, 1.0, 1.0</td>
<td align="left">NaN, NaN, NaN</td>
<td align="left">NaN, NaN, NaN</td>
<td align="left">NA, NA, NA</td>
<td align="left">NA, NA, NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left"><span class="math inline"><em>θ</em></span></th>
<th align="left"><span class="math inline"><em>ξ</em></span></th>
<th align="left">Conditional to</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Asymptotic Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">1, 1, 1</td>
<td align="left">2.220, 0.796, 1.790</td>
<td align="left">344.0, -20.4, -10.6</td>
<td align="left">68.79590, 3.05235, 5.05403</td>
<td align="left">0.03614270, 0.01484920, 0.00756908</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">1, 1, 1</td>
<td align="left">0.50, 1.00, 1.98</td>
<td align="left">0.0543, 0.3690, -0.8880</td>
<td align="left">1.63399, 1.88679, 1.60761</td>
<td align="left">0.0712723, 0.0348923, 0.0163413</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">1, 1, 1</td>
<td align="left">0.513, 0.993, 1.990</td>
<td align="left">2.600, -0.661, -0.330</td>
<td align="left">1, 1, 1</td>
<td align="left">0.0434493, 0.0184565, 0.0103178</td>
<td align="left">0.0416175, 0.0177885, 0.0108431</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">1, 1, 1</td>
<td align="left">NaN, NaN, NaN</td>
<td align="left">NaN, NaN, NaN</td>
<td align="left">NA, NA, NA</td>
<td align="left">NA, NA, NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left"><span class="math inline"><em>θ</em></span></th>
<th align="left"><span class="math inline"><em>ξ</em></span></th>
<th align="left">Conditional to</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Asymptotic Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">10, 1, 1</td>
<td align="left">1.140, 0.967, 1.960</td>
<td align="left">128.00, -3.30, -1.83</td>
<td align="left">8.821740, 0.997874, 1.050360</td>
<td align="left">0.03524990, 0.01565320, 0.00772124</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">10, 1, 1</td>
<td align="left">0.499, 1.000, 1.980</td>
<td align="left">-0.108, 0.233, -1.180</td>
<td align="left">1.64251, 2.37836, 2.27889</td>
<td align="left">0.0832800, 0.0398984, 0.0191108</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">10, 1, 1</td>
<td align="left">0.50, 1.00, 1.99</td>
<td align="left">0.0513, 0.1220, -0.5050</td>
<td align="left">1, 1, 1</td>
<td align="left">0.05070290, 0.01677640, 0.00853041</td>
<td align="left">0.05223770, 0.01538530, 0.00909232</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">0.5, 1.0, 2.0</td>
<td align="left">2</td>
<td align="left">10, 1, 1</td>
<td align="left">NaN, NaN, NaN</td>
<td align="left">NaN, NaN, NaN</td>
<td align="left">NA, NA, NA</td>
<td align="left">NA, NA, NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

### Table 3

<table>
<thead>
<tr class="header">
<th align="left">Selection</th>
<th align="left">Estimator</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Average Estimated Variance</th>
<th align="left">Variance Ratio</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Unstratified</td>
<td align="left">Naive</td>
<td align="left">36.949</td>
<td align="left">-7.286</td>
<td align="left">20.995</td>
<td align="left">0.188</td>
<td align="left">0.186</td>
<td align="left">0.989</td>
</tr>
<tr class="even">
<td align="left">Unstratified</td>
<td align="left">Pseudo</td>
<td align="left">39.805</td>
<td align="left">-0.122</td>
<td align="left">1.106</td>
<td align="left">0.452</td>
<td align="left">0.419</td>
<td align="left">0.926</td>
</tr>
<tr class="odd">
<td align="left">Unstratified</td>
<td align="left">Sample</td>
<td align="left">39.827</td>
<td align="left">-0.065</td>
<td align="left">1</td>
<td align="left">0.41</td>
<td align="left">0.388</td>
<td align="left">0.945</td>
</tr>
<tr class="even">
<td align="left">Stratified</td>
<td align="left">Naive</td>
<td align="left">36.932</td>
<td align="left">-7.328</td>
<td align="left">113.911</td>
<td align="left">0.006</td>
<td align="left">0.188</td>
<td align="left">30.271</td>
</tr>
<tr class="odd">
<td align="left">Stratified</td>
<td align="left">Pseudo</td>
<td align="left">39.858</td>
<td align="left">0.013</td>
<td align="left">2.448</td>
<td align="left">0.184</td>
<td align="left">0.169</td>
<td align="left">0.923</td>
</tr>
<tr class="even">
<td align="left">Stratified</td>
<td align="left">Sample</td>
<td align="left">39.848</td>
<td align="left">-0.012</td>
<td align="left">1</td>
<td align="left">0.075</td>
<td align="left">0.066</td>
<td align="left">0.886</td>
</tr>
</tbody>
</table>
