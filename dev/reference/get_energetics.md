# Calculate ATP Production from OXPHOS and Glycolysis

Calculates ATP production from glycolysis and OXPHOS at points defined
in patitioned_data

## Usage

``` r
get_energetics(partitioned_data, ph, pka, buffer)
```

## Arguments

- partitioned_data:

  a data.table of organized Seahorse OCR and ECAR rates based on
  timepoints from the assay cycle. Returned by `partition_data`

- ph:

  pH value for energetics calculation (for XF Media, 7.5)

- pka:

  pKa value for energetics calculation (for XF Media, 6.063)

- buffer:

  buffer for energetics calculation (for XF Media, 0.1 mpH/pmol H+)

## Value

a `data.table` of glycolysis and OXPHOS rates

## Details

TODO: check that all symbols are defined

Proton production rate (PPR): \$\$\text{PPR} = \frac{\text{ECAR
value}}{\text{buffer}}\$\$

\$\$ \text{PPR}\_{\text{mito}} =
\frac{10^{\text{pH}-\text{pK}\_a}}{1+10^{\text{pH}-\text{pK}\_a}} \cdot
\frac{\text{H}^+}{\text{O}\_2} \cdot \text{OCR} \$\$

calculates the proton production from glucose during its conversion to
bicarbonate and \\\text{H}^+\\ assuming max
\\\frac{\text{H}^+}{\text{O}\_2}\\ of 1

\$\$ \text{PPR}\_\text{glyc} = \text{PPR} - \text{PPR}\_\text{resp} \$\$

calculates the proton production from glucose during its conversion to
lactate + \\\text{H}^+\\

Joules of ATP (JATP) production:

\$\$ \text{ATP}\_{\text{glyc}} = \Bigl(\text{PPR}\_\text{glyc} \cdot
\frac{\text{ATP}}{\text{lactate}}\Bigl) + \Bigl(\text{MITO}\_\text{resp}
\cdot 2 \cdot \frac{\text{P}}{\text{O}\_\text{glyc}}\Bigl) \$\$

\$\$ \frac{\text{ATP}}{\text{lactate}} = 1 \$\$ with
\\\frac{\text{P}}{{\text{O}\_\text{glyc}}}\\ = 0.167 for glucose (0.242
for glycogen).

\$\$ \text{ATP}\_\text{resp} = \Bigl(\text{coupled MITO}\_\text{resp}
\cdot 2 \cdot \frac{\text{P}}{\text{O}\_\text{oxphos}}\Bigl) +
\Bigl(\text{MITO}\_\text{resp} \cdot 2 \cdot
\frac{\text{P}}{\text{O}\_\text{TCA}}\Bigl) \$\$ with
\\\frac{\text{P}}{{\text{O}\_\text{oxphos}}}\\ = 2.486 and
\\\frac{\text{P}}{{\text{O}\_\text{TCA}}}\\ = 0.167.

## Examples

``` r
rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)
seahorse_rates <- read_data(rep_list, sheet = 2)
partitioned_data <- partition_data(seahorse_rates)
energetics <- get_energetics(
  partitioned_data,
  ph = 7.4,
  pka = 6.093,
  buffer = 0.1
)
head(energetics, n = 10)
#>     exp_group replicate ATP_basal_resp ATP_max_resp ATP_basal_glyc ATP_max_glyc
#>        <fctr>    <fctr>          <num>        <num>          <num>        <num>
#>  1:   Group_1         1       1083.877     1125.075      176.42439     524.6518
#>  2:   Group_1         1       1080.106     1117.407      129.69559     442.5421
#>  3:   Group_1         1       1233.324     1278.141      141.43402     472.4431
#>  4:   Group_1         2       1002.058     1042.796       95.13176     390.3254
#>  5:   Group_1         2       1066.278     1107.473       87.08441     420.6239
#>  6:   Group_1         2       1088.996     1127.567      107.80548     485.7402
#>  7:   Group_1         2       1078.165     1114.478      127.14102     469.7935
#>  8:   Group_1         2       1127.103     1172.258      114.74865     477.7720
#>  9:   Group_1         2       1022.219     1058.623      137.66492     504.0603
#> 10:   Group_1         2       1152.477     1195.666      103.91464     426.4569
```
