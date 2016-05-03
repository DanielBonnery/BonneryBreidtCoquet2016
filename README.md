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

    ## Warning in data(table1, table2, table3): data set 'table2' not found

### Table 1

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left">Mean</th>
<th align="left">X..Relative.Bias</th>
<th align="left">RMSE.Ratio</th>
<th align="left">Empirical.Variance</th>
<th align="left">Asymptotic.Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">theta.bar</td>
<td align="left">1.32943</td>
<td align="left">-200.8807</td>
<td align="left">1406.214</td>
<td align="left">8.664352e-06</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">theta.ht</td>
<td align="left">4.070556</td>
<td align="left">1.733317</td>
<td align="left">6.650471</td>
<td align="left">0.02875136</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">theta.hat</td>
<td align="left">4.016571</td>
<td align="left">0.4125647</td>
<td align="left">1</td>
<td align="left">0.004797144</td>
<td align="left">1</td>
</tr>
<tr class="even">
<td align="left">theta.full</td>
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
<th align="left">X..Relative.Bias</th>
<th align="left">RMSE.Ratio</th>
<th align="left">Empirical.Variance</th>
<th align="left">Asymptotic.Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">theta.bar</td>
<td align="left">1.32927</td>
<td align="left">-200.9171</td>
<td align="left">116.1432</td>
<td align="left">1.852971e-05</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">theta.ht</td>
<td align="left">4.005045</td>
<td align="left">0.1259766</td>
<td align="left">1.700214</td>
<td align="left">0.1043915</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">theta.hat</td>
<td align="left">4.098965</td>
<td align="left">2.414393</td>
<td align="left">1</td>
<td align="left">0.05161991</td>
<td align="left">1</td>
</tr>
<tr class="even">
<td align="left">theta.full</td>
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
<th align="left">X..Relative.Bias</th>
<th align="left">RMSE.Ratio</th>
<th align="left">Empirical.Variance</th>
<th align="left">Asymptotic.Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">theta.bar</td>
<td align="left">1.331841</td>
<td align="left">-200.3361</td>
<td align="left">419.8108</td>
<td align="left">3.010852e-05</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">theta.ht</td>
<td align="left">4.064297</td>
<td align="left">1.581987</td>
<td align="left">7.594252</td>
<td align="left">0.1246484</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">theta.hat</td>
<td align="left">4.1252</td>
<td align="left">3.035004</td>
<td align="left">1</td>
<td align="left">0.001282842</td>
<td align="left">1</td>
</tr>
<tr class="even">
<td align="left">theta.full</td>
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
