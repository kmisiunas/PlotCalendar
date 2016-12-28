# PlotCalendar

Plots calender heat map from TimeSeries object for Wolram Language/Mathematica

## Example

```
ed = EarthquakeData[All, 6, {{2014, 1, 1}, {2016, 1, 1}}, "Magnitude"];
PlotCalendar[ed, "Years" -> {2014, 2015},  ColorFunction -> "Rainbow"]
```

![Output graphics](https://cloud.githubusercontent.com/assets/4820843/21533511/ec056260-cd63-11e6-8092-fe8e569df615.png)