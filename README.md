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

### Table 3

    library(knitr)
    #kable(table1)
    kable(table3)

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
<td align="left">36.9493341452716</td>
<td align="left">-7.28594046804115</td>
<td align="left">20.9946202438705</td>
<td align="left">0.187706107401689</td>
<td align="left">0.185605089100079</td>
<td align="left">0.988806872985154</td>
</tr>
<tr class="even">
<td align="left">Unstratified</td>
<td align="left">Pseudo</td>
<td align="left">39.8045284956133</td>
<td align="left">-0.121625735544975</td>
<td align="left">1.10617455375691</td>
<td align="left">0.452213840893838</td>
<td align="left">0.418543361126754</td>
<td align="left">0.925543013675717</td>
</tr>
<tr class="odd">
<td align="left">Unstratified</td>
<td align="left">Sample</td>
<td align="left">39.8271511013294</td>
<td align="left">-0.0648606094161167</td>
<td align="left">1</td>
<td align="left">0.410266020257011</td>
<td align="left">0.387829030084481</td>
<td align="left">0.94531111750743</td>
</tr>
<tr class="even">
<td align="left">Stratified</td>
<td align="left">Naive</td>
<td align="left">36.9324436637606</td>
<td align="left">-7.32832242551223</td>
<td align="left">113.910998791737</td>
<td align="left">0.0061980608369084</td>
<td align="left">0.187623391642629</td>
<td align="left">30.2713052646021</td>
</tr>
<tr class="odd">
<td align="left">Stratified</td>
<td align="left">Pseudo</td>
<td align="left">39.8582352344487</td>
<td align="left">0.0131363622530285</td>
<td align="left">2.44823123162282</td>
<td align="left">0.183612703313765</td>
<td align="left">0.16939004440432</td>
<td align="left">0.922539897007337</td>
</tr>
<tr class="even">
<td align="left">Stratified</td>
<td align="left">Sample</td>
<td align="left">39.8482792387455</td>
<td align="left">-0.0118454351102767</td>
<td align="left">1</td>
<td align="left">0.074987003340516</td>
<td align="left">0.0664162043349024</td>
<td align="left">0.885702873514046</td>
</tr>
</tbody>
</table>
