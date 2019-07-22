# gbiqqtools

`gbiqqtools` is a set of functions for choosing research strategies. 

## Clue selection
Drawing on a causal-model-based approach we can figure out how informative a clue is given the data we already have.





```
library(dplyr)
model <-
   make_model("X->M->Y")  %>%
   set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
   set_parameter_matrix()
    
 conditional_inferences(model, query = "Y[X=1]>Y[X=0]", given = "Y==1")
 expected_learning(model, query = "Y[X=1]>Y[X=0]", strategy = c("1"), given = "Y==1")
```


## Case selection

```
analysis_model <- reference_model <- model
df             <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
given          <- collapse_data(df, reference_model)
diagnose_strategies(reference_model = reference_model,
                    analysis_model = analysis_model,
                    given = given,
                    queries ="Y[X=1]>Y[X=0]",
                    sims = 10)
```
