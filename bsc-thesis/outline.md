- **Introduction**
  - Motivation
    - Google test engineer blog post
  - RQ
    - What are the perfomances of classifiers in predicting flaky tests based on JVM metrics?
    - Which classifiers perform best especially parametric versus non-parametric?

- **Related Work**
  - See thesis
    - Categorization
      - various papers
    - Tools already developed
      - DeFlaker
      - iDFlakies

- **Method**
  - Describe the highlevel workflow
  - Data set from iDFlakies
    - Explanation of the data set
    - Summary of projects tests etc.
  - Toolchain
    - Flow diagram
    - iDFlakies data set
    - Code injection
      - Dropwizard library
    - ScienceCloud
      - runtime environment
  - Feature selection
    - CFS and BestFirst search algorithm
  - Classifiers
    - listing of classifiers (not detailed???)
  - Cross-validation
    - Prior knowledge that data will be imbalanced
    - Stratified cross-validation a way to deal with imbalanced data set

- **Results**
  - PCA biplots (dimension reduction)
    - Machine A
      - own identified
        - absolute values
        - diff values
      - idflakies
        - absolute values
        - diff values
      - union
        - absolute values
        - diff values

    - Machine B
      - own identified
        - absolute values
        - diff values
      - idflakies
        - absolute values
        - diff values
      - union
        - absolute values
        - diff values

    - Special union (synchronized labels among machine A and B)
      - absolute values
      - diff values

  - Cross-validation (CFS not balanced)
    - Machine A
      - own identified
        - absolute values
        - diff values
      - idflakies
        - absolute values
        - diff values
      - union
        - absolute values
        - diff values

    - Machine B
      - own identified
        - absolute values
        - diff values
      - idflakies
        - absolute values
        - diff values
      - union
        - absolute values
        - diff values

    - Special union (synchronized labels among machine A and B)
      - absolute values
      - diff values

- **Discussion**
  - Bigger picture
    - Is it worth to do more research on flaky tests based on JVM metrics?
    - Developing tools based on the insights of this thesis
    - Impact on SE in general
- **Conclusion**
  - Summary of the main insights
- **Appendix**


