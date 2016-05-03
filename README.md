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
<th align="left"></th>
<th align="right">Y</th>
<th align="left"></th>
<th align="left"></th>
<th align="left"></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Naive</td>
<td align="left">36.94933</td>
<td align="right">-7.2859405</td>
<td align="left">20.994620</td>
<td align="left">0.1877061</td>
<td align="left">0.1856051</td>
</tr>
<tr class="even">
<td align="left">Pseudo</td>
<td align="left">39.80453</td>
<td align="right">-0.1216257</td>
<td align="left">1.106175</td>
<td align="left">0.4522138</td>
<td align="left">0.4185434</td>
</tr>
<tr class="odd">
<td align="left">Sample</td>
<td align="left">39.82715</td>
<td align="right">-0.0648606</td>
<td align="left">1.000000</td>
<td align="left">0.4102660</td>
<td align="left">0.3878290</td>
</tr>
<tr class="even">
<td align="left">Naive</td>
<td align="left">36.93244</td>
<td align="right">-7.3283224</td>
<td align="left">113.910999</td>
<td align="left">0.0061981</td>
<td align="left">0.1876234</td>
</tr>
<tr class="odd">
<td align="left">Pseudo</td>
<td align="left">39.85824</td>
<td align="right">0.0131364</td>
<td align="left">2.448231</td>
<td align="left">0.1836127</td>
<td align="left">0.1693900</td>
</tr>
<tr class="even">
<td align="left">Sample</td>
<td align="left">39.84828</td>
<td align="right">-0.0118454</td>
<td align="left">1.000000</td>
<td align="left">0.0749870</td>
<td align="left">0.0664162</td>
</tr>
</tbody>
</table>
