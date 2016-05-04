Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population
============================================================================================================

`pubBonneryBreidtCoquet2016` is a R package that contains the code to
reproduce the simulations of ["Asymptotics for the maximum sample
likelihood estimator under informative selection from a finite
population"](http://www.e-publications.org/ims/submission/BEJ/user/submissionFile/23537?confirm=3b2ff5b3)
by Bonnery, Breidt and Coquet

Execution
---------

Execute in R:

    devtools::github("DanielBonnery/pubBonneryBreidtCoquet2016")
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
<td align="left">4.1</td>
<td align="left">2.35</td>
<td align="left">1.35614</td>
<td align="left">0.0176681</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">4.01</td>
<td align="left">0.282</td>
<td align="left">1.57986</td>
<td align="left">0.0312076</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">4.01</td>
<td align="left">0.153</td>
<td align="left">1</td>
<td align="left">0.0197967</td>
<td align="left">0.0197794</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">NA</td>
<td align="left">NA</td>
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
<td align="left">4.85</td>
<td align="left">17.6</td>
<td align="left">15.5287</td>
<td align="left">0.031158</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">4.04</td>
<td align="left">1.1</td>
<td align="left">2.21818</td>
<td align="left">0.106808</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">4.02</td>
<td align="left">0.374</td>
<td align="left">1</td>
<td align="left">0.0488144</td>
<td align="left">0.0429614</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">NA</td>
<td align="left">NA</td>
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
<td align="left">5.58</td>
<td align="left">28.3</td>
<td align="left">33.2311</td>
<td align="left">0.0545275</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">4.05</td>
<td align="left">1.27</td>
<td align="left">1.83004</td>
<td align="left">0.137634</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">4.02</td>
<td align="left">0.391</td>
<td align="left">1</td>
<td align="left">0.0764089</td>
<td align="left">0.100191</td>
</tr>
<tr class="even">
<td align="left">Full</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

### Table 2

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
