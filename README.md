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

    `devtools::github("DanielBonnery/pubBonneryBreidtCoquet2016")`
    library(pubBonneryBreidtCoquet2016)
    demo(table1)
    demo(table2)
    demo(table3)
    library(knitr)
    kable(table3)

    library(pubBonneryBreidtCoquet2016)
    #data(table1)
    data(table3)
    library(knitr)
    #kable(table1)
    kable(table3)

<table>
<thead>
<tr class="header">
<th align="left"></th>
<th align="right">Mean</th>
<th align="right">% Relative Bias</th>
<th align="right">RMSE Ratio</th>
<th align="right">Empirical Variance</th>
<th align="right">Average Estimated Variance</th>
<th align="right">Variance Ratio</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="right">36.94933</td>
<td align="right">-7.2859405</td>
<td align="right">20.994620</td>
<td align="right">0.1877061</td>
<td align="right">0.1856051</td>
<td align="right">0.9888069</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="right">39.80453</td>
<td align="right">-0.1216257</td>
<td align="right">1.106175</td>
<td align="right">0.4522138</td>
<td align="right">0.4185434</td>
<td align="right">0.9255430</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="right">39.82715</td>
<td align="right">-0.0648606</td>
<td align="right">1.000000</td>
<td align="right">0.4102660</td>
<td align="right">0.3878290</td>
<td align="right">0.9453111</td>
</tr>
<tr class="even">
<td align="left">Naive</td>
<td align="right">36.93244</td>
<td align="right">-7.3283224</td>
<td align="right">113.910999</td>
<td align="right">0.0061981</td>
<td align="right">0.1876234</td>
<td align="right">30.2713053</td>
</tr>
<tr class="odd">
<td align="left">Pseudo</td>
<td align="right">39.85824</td>
<td align="right">0.0131364</td>
<td align="right">2.448231</td>
<td align="right">0.1836127</td>
<td align="right">0.1693900</td>
<td align="right">0.9225399</td>
</tr>
<tr class="even">
<td align="left">Sample</td>
<td align="right">39.84828</td>
<td align="right">-0.0118454</td>
<td align="right">1.000000</td>
<td align="right">0.0749870</td>
<td align="right">0.0664162</td>
<td align="right">0.8857029</td>
</tr>
</tbody>
</table>
