{
 "cells": [
  {
   "cell_type": "markdown",
   "id": "08c23a0f",
   "metadata": {
    "papermill": {
     "duration": 0.007983,
     "end_time": "2024-07-27T21:21:50.998577",
     "exception": false,
     "start_time": "2024-07-27T21:21:50.990594",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Introduction \n",
    "\n",
    "In this notebook we will implementation advanced statistical techniques such as Two-way ANOVA analysis on an Experiment that examines the impact of educational aspirations, specifically how academic performance is influenced by a person's ambition to seek higher education.\n",
    "\n",
    "ANOVA - Analysis of variance is used to test whether there is statistically significant difference between mean of three or more samples. A Two-way ANOVA method is used to test the effect of two categorical variables (Independent/factors) on a continuous variable (Dependent).\n",
    "\n",
    "I used a student performance dataset found online on Kaggle for the purpose of the project. The data is of student academic performance in secondary education of two Portuguese schools in Portuguese subject. The data consists of student grades, their demographic, social and school related information as features and it was gathered by surveys and questionnaires in school campuses.\n",
    "\n",
    "# The problem statement of the experiment is as follows:\n",
    "\n",
    "How does the willingness to pursue higher education among different genders influence their academic performance?\n",
    "\n",
    "With the help of statistical methods like Two-way ANOVA we will find if there is any effect of gender and willingness to pursue higher education on their final grades."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 1,
   "id": "a4f4cf19",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:51.017771Z",
     "iopub.status.busy": "2024-07-27T21:21:51.015345Z",
     "iopub.status.idle": "2024-07-27T21:21:54.346812Z",
     "shell.execute_reply": "2024-07-27T21:21:54.344810Z"
    },
    "papermill": {
     "duration": 3.344006,
     "end_time": "2024-07-27T21:21:54.349882",
     "exception": false,
     "start_time": "2024-07-27T21:21:51.005876",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "Loading required package: carData\n",
      "\n"
     ]
    }
   ],
   "source": [
    "#Importing required Libraries\n",
    "library(readr)\n",
    "library(ggplot2)\n",
    "library(car)\n",
    "library(Matrix)\n",
    "library(ARTool)\n",
    "library(stats)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "784f223a",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:54.402752Z",
     "iopub.status.busy": "2024-07-27T21:21:54.367230Z",
     "iopub.status.idle": "2024-07-27T21:21:54.954258Z",
     "shell.execute_reply": "2024-07-27T21:21:54.952248Z"
    },
    "papermill": {
     "duration": 0.59942,
     "end_time": "2024-07-27T21:21:54.957102",
     "exception": false,
     "start_time": "2024-07-27T21:21:54.357682",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "\u001b[1m\u001b[22mNew names:\n",
      "\u001b[36m•\u001b[39m `` -> `...1`\n"
     ]
    },
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "\u001b[1mRows: \u001b[22m\u001b[34m649\u001b[39m \u001b[1mColumns: \u001b[22m\u001b[34m34\u001b[39m\n"
     ]
    },
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "\u001b[36m──\u001b[39m \u001b[1mColumn specification\u001b[22m \u001b[36m────────────────────────────────────────────────────────\u001b[39m\n",
      "\u001b[1mDelimiter:\u001b[22m \",\"\n",
      "\u001b[31mchr\u001b[39m (17): school, sex, address, famsize, Pstatus, Mjob, Fjob, reason, guardi...\n",
      "\u001b[32mdbl\u001b[39m (17): ...1, age, Medu, Fedu, traveltime, studytime, failures, famrel, fr...\n"
     ]
    },
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "\n",
      "\u001b[36mℹ\u001b[39m Use `spec()` to retrieve the full column specification for this data.\n",
      "\u001b[36mℹ\u001b[39m Specify the column types or set `show_col_types = FALSE` to quiet this message.\n"
     ]
    },
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A tibble: 6 × 34</caption>\n",
       "<thead>\n",
       "\t<tr><th scope=col>...1</th><th scope=col>school</th><th scope=col>sex</th><th scope=col>age</th><th scope=col>address</th><th scope=col>famsize</th><th scope=col>Pstatus</th><th scope=col>Medu</th><th scope=col>Fedu</th><th scope=col>Mjob</th><th scope=col>⋯</th><th scope=col>famrel</th><th scope=col>freetime</th><th scope=col>goout</th><th scope=col>Dalc</th><th scope=col>Walc</th><th scope=col>health</th><th scope=col>absences</th><th scope=col>G1</th><th scope=col>G2</th><th scope=col>G3</th></tr>\n",
       "\t<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>⋯</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><td>0</td><td>GP</td><td>F</td><td>18</td><td>U</td><td>GT3</td><td>A</td><td>4</td><td>4</td><td>at_home </td><td>⋯</td><td>4</td><td>3</td><td>4</td><td>1</td><td>1</td><td>3</td><td>4</td><td> 0</td><td>11</td><td>11</td></tr>\n",
       "\t<tr><td>1</td><td>GP</td><td>F</td><td>17</td><td>U</td><td>GT3</td><td>T</td><td>1</td><td>1</td><td>at_home </td><td>⋯</td><td>5</td><td>3</td><td>3</td><td>1</td><td>1</td><td>3</td><td>2</td><td> 9</td><td>11</td><td>11</td></tr>\n",
       "\t<tr><td>2</td><td>GP</td><td>F</td><td>15</td><td>U</td><td>LE3</td><td>T</td><td>1</td><td>1</td><td>at_home </td><td>⋯</td><td>4</td><td>3</td><td>2</td><td>2</td><td>3</td><td>3</td><td>6</td><td>12</td><td>13</td><td>12</td></tr>\n",
       "\t<tr><td>3</td><td>GP</td><td>F</td><td>15</td><td>U</td><td>GT3</td><td>T</td><td>4</td><td>2</td><td>health  </td><td>⋯</td><td>3</td><td>2</td><td>2</td><td>1</td><td>1</td><td>5</td><td>0</td><td>14</td><td>14</td><td>14</td></tr>\n",
       "\t<tr><td>4</td><td>GP</td><td>F</td><td>16</td><td>U</td><td>GT3</td><td>T</td><td>3</td><td>3</td><td>other   </td><td>⋯</td><td>4</td><td>3</td><td>2</td><td>1</td><td>2</td><td>5</td><td>0</td><td>11</td><td>13</td><td>13</td></tr>\n",
       "\t<tr><td>5</td><td>GP</td><td>M</td><td>16</td><td>U</td><td>LE3</td><td>T</td><td>4</td><td>3</td><td>services</td><td>⋯</td><td>5</td><td>4</td><td>2</td><td>1</td><td>2</td><td>5</td><td>6</td><td>12</td><td>12</td><td>13</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A tibble: 6 × 34\n",
       "\\begin{tabular}{lllllllllllllllllllll}\n",
       " ...1 & school & sex & age & address & famsize & Pstatus & Medu & Fedu & Mjob & ⋯ & famrel & freetime & goout & Dalc & Walc & health & absences & G1 & G2 & G3\\\\\n",
       " <dbl> & <chr> & <chr> & <dbl> & <chr> & <chr> & <chr> & <dbl> & <dbl> & <chr> & ⋯ & <dbl> & <dbl> & <dbl> & <dbl> & <dbl> & <dbl> & <dbl> & <dbl> & <dbl> & <dbl>\\\\\n",
       "\\hline\n",
       "\t 0 & GP & F & 18 & U & GT3 & A & 4 & 4 & at\\_home  & ⋯ & 4 & 3 & 4 & 1 & 1 & 3 & 4 &  0 & 11 & 11\\\\\n",
       "\t 1 & GP & F & 17 & U & GT3 & T & 1 & 1 & at\\_home  & ⋯ & 5 & 3 & 3 & 1 & 1 & 3 & 2 &  9 & 11 & 11\\\\\n",
       "\t 2 & GP & F & 15 & U & LE3 & T & 1 & 1 & at\\_home  & ⋯ & 4 & 3 & 2 & 2 & 3 & 3 & 6 & 12 & 13 & 12\\\\\n",
       "\t 3 & GP & F & 15 & U & GT3 & T & 4 & 2 & health   & ⋯ & 3 & 2 & 2 & 1 & 1 & 5 & 0 & 14 & 14 & 14\\\\\n",
       "\t 4 & GP & F & 16 & U & GT3 & T & 3 & 3 & other    & ⋯ & 4 & 3 & 2 & 1 & 2 & 5 & 0 & 11 & 13 & 13\\\\\n",
       "\t 5 & GP & M & 16 & U & LE3 & T & 4 & 3 & services & ⋯ & 5 & 4 & 2 & 1 & 2 & 5 & 6 & 12 & 12 & 13\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A tibble: 6 × 34\n",
       "\n",
       "| ...1 &lt;dbl&gt; | school &lt;chr&gt; | sex &lt;chr&gt; | age &lt;dbl&gt; | address &lt;chr&gt; | famsize &lt;chr&gt; | Pstatus &lt;chr&gt; | Medu &lt;dbl&gt; | Fedu &lt;dbl&gt; | Mjob &lt;chr&gt; | ⋯ ⋯ | famrel &lt;dbl&gt; | freetime &lt;dbl&gt; | goout &lt;dbl&gt; | Dalc &lt;dbl&gt; | Walc &lt;dbl&gt; | health &lt;dbl&gt; | absences &lt;dbl&gt; | G1 &lt;dbl&gt; | G2 &lt;dbl&gt; | G3 &lt;dbl&gt; |\n",
       "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n",
       "| 0 | GP | F | 18 | U | GT3 | A | 4 | 4 | at_home  | ⋯ | 4 | 3 | 4 | 1 | 1 | 3 | 4 |  0 | 11 | 11 |\n",
       "| 1 | GP | F | 17 | U | GT3 | T | 1 | 1 | at_home  | ⋯ | 5 | 3 | 3 | 1 | 1 | 3 | 2 |  9 | 11 | 11 |\n",
       "| 2 | GP | F | 15 | U | LE3 | T | 1 | 1 | at_home  | ⋯ | 4 | 3 | 2 | 2 | 3 | 3 | 6 | 12 | 13 | 12 |\n",
       "| 3 | GP | F | 15 | U | GT3 | T | 4 | 2 | health   | ⋯ | 3 | 2 | 2 | 1 | 1 | 5 | 0 | 14 | 14 | 14 |\n",
       "| 4 | GP | F | 16 | U | GT3 | T | 3 | 3 | other    | ⋯ | 4 | 3 | 2 | 1 | 2 | 5 | 0 | 11 | 13 | 13 |\n",
       "| 5 | GP | M | 16 | U | LE3 | T | 4 | 3 | services | ⋯ | 5 | 4 | 2 | 1 | 2 | 5 | 6 | 12 | 12 | 13 |\n",
       "\n"
      ],
      "text/plain": [
       "  ...1 school sex age address famsize Pstatus Medu Fedu Mjob     ⋯ famrel\n",
       "1 0    GP     F   18  U       GT3     A       4    4    at_home  ⋯ 4     \n",
       "2 1    GP     F   17  U       GT3     T       1    1    at_home  ⋯ 5     \n",
       "3 2    GP     F   15  U       LE3     T       1    1    at_home  ⋯ 4     \n",
       "4 3    GP     F   15  U       GT3     T       4    2    health   ⋯ 3     \n",
       "5 4    GP     F   16  U       GT3     T       3    3    other    ⋯ 4     \n",
       "6 5    GP     M   16  U       LE3     T       4    3    services ⋯ 5     \n",
       "  freetime goout Dalc Walc health absences G1 G2 G3\n",
       "1 3        4     1    1    3      4         0 11 11\n",
       "2 3        3     1    1    3      2         9 11 11\n",
       "3 3        2     2    3    3      6        12 13 12\n",
       "4 2        2     1    1    5      0        14 14 14\n",
       "5 3        2     1    2    5      0        11 13 13\n",
       "6 4        2     1    2    5      6        12 12 13"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#loading data\n",
    "data <- read_csv(\"/kaggle/input/student-performance-prediction/por2.csv\")\n",
    "head(data)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "7ce77e91",
   "metadata": {
    "papermill": {
     "duration": 0.008503,
     "end_time": "2024-07-27T21:21:54.973874",
     "exception": false,
     "start_time": "2024-07-27T21:21:54.965371",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Factors and Levels\n",
    "\n",
    "The factors being studied are as follows:\n",
    "\n",
    "* Factor 1: Sex - student's sex which are binary: \"F\" - female or \"M\" – male.\n",
    "* Factor 2: Higher - wants to take higher education which are binary: yes or no.\n",
    "* Dependent variable: G3 - final grade which are in numeric: from 0 to 20."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "id": "6bfa7d79",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:54.994723Z",
     "iopub.status.busy": "2024-07-27T21:21:54.992222Z",
     "iopub.status.idle": "2024-07-27T21:21:55.017423Z",
     "shell.execute_reply": "2024-07-27T21:21:55.015116Z"
    },
    "papermill": {
     "duration": 0.038813,
     "end_time": "2024-07-27T21:21:55.020800",
     "exception": false,
     "start_time": "2024-07-27T21:21:54.981987",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "E1 <- subset(data, select = c(sex,higher,G3))\n",
    "E1$sex <- as.factor(E1$sex)\n",
    "E1$higher <- as.factor(E1$higher)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "cd97e43f",
   "metadata": {
    "papermill": {
     "duration": 0.008033,
     "end_time": "2024-07-27T21:21:55.037465",
     "exception": false,
     "start_time": "2024-07-27T21:21:55.029432",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Hypotheses:\n",
    "\n",
    "There are three Null hypotheses and there also three alternative hypotheses\n",
    "\n",
    "## Null hypotheses\n",
    "1. There is no difference between the groups of the independent variable sex in relation to the dependent variable G3.\n",
    "2. There is no difference between the groups of the independent variable higher in relation to the dependent variable G3.\n",
    "3. There is no interaction between the two variables sex and higher in relation to the dependent variable G3.\n",
    "\n",
    "## Alternative hypotheses\n",
    "1. There is a difference between the groups of the independent variable sex in relation to the dependent variable G3.\n",
    "2. There is a difference between the groups of the independent variable higher in relation to the dependent variable G3.\n",
    "3. There is an interaction between the two variables sex and higher in relation to the dependent variable G3."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "id": "5a8e8bf4",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:55.058862Z",
     "iopub.status.busy": "2024-07-27T21:21:55.056186Z",
     "iopub.status.idle": "2024-07-27T21:21:55.100714Z",
     "shell.execute_reply": "2024-07-27T21:21:55.098299Z"
    },
    "papermill": {
     "duration": 0.058405,
     "end_time": "2024-07-27T21:21:55.104002",
     "exception": false,
     "start_time": "2024-07-27T21:21:55.045597",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "             Df Sum Sq Mean Sq F value   Pr(>F)    \n",
       "sex           1    113   112.7  12.248 0.000498 ***\n",
       "higher        1    715   715.3  77.753  < 2e-16 ***\n",
       "sex:higher    1      1     1.2   0.135 0.713359    \n",
       "Residuals   645   5934     9.2                     \n",
       "---\n",
       "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "M1 <- aov(G3 ~ sex * higher, data = E1)\n",
    "summary(M1)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "ea47194e",
   "metadata": {
    "papermill": {
     "duration": 0.009177,
     "end_time": "2024-07-27T21:21:55.121368",
     "exception": false,
     "start_time": "2024-07-27T21:21:55.112191",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Assumption Checks:\n",
    "For the test results of Two-way ANOVA to be valid several assumptions must be met. Those are as follows:\n",
    "* Normality:\n",
    "\n",
    "The data within the group should be normally distributed or the residuals should be normally distributed. This can be checked with a Quantile-Quantile plot and histogram plot respectively as below.\n",
    "\n",
    "In the histogram plot, we can see the data is negatively skewed (long tail to the left). Also, with applying Shapiro-Wilk Test on Residuals. We get p-value = 3.655e-16, which is < 0.05 implying the residuals do not follow the normal distribution rejecting null hypotheses.\n",
    "\n",
    "Since our data failed to normality assumption. We will move to non-parametric alternative like the Kruskal-Wallis test. However, even though Kruskal-Wallis test relaxes the normality assumption still it needs at least one factor to be ordinally measured. Also, it is ideal for one factor analysis.\n",
    "\n",
    "In our case, both our factors are nominal and the dependent variable is metric and can be considered as ordinal. We will procced to Aligned Rank Transform Test (ART). Which is useful to handle data interaction between factor, important for two-way design. However, we will continue to check if the variance of our data groups is equal."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "id": "bf96c351",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:55.143365Z",
     "iopub.status.busy": "2024-07-27T21:21:55.140510Z",
     "iopub.status.idle": "2024-07-27T21:21:55.909261Z",
     "shell.execute_reply": "2024-07-27T21:21:55.906955Z"
    },
    "papermill": {
     "duration": 0.782525,
     "end_time": "2024-07-27T21:21:55.912190",
     "exception": false,
     "start_time": "2024-07-27T21:21:55.129665",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "Warning message:\n",
      "“\u001b[1m\u001b[22mUsing `size` aesthetic for lines was deprecated in ggplot2 3.4.0.\n",
      "\u001b[36mℹ\u001b[39m Please use `linewidth` instead.”\n"
     ]
    },
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "Warning message:\n",
      "“\u001b[1m\u001b[22mThe dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.\n",
      "\u001b[36mℹ\u001b[39m Please use `after_stat(density)` instead.”\n"
     ]
    },
    {
     "data": {
      "image/png": "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"
     },
     "metadata": {
      "image/png": {
       "height": 420,
       "width": 420
      }
     },
     "output_type": "display_data"
    }
   ],
   "source": [
    "#Histogram plot\n",
    "mean_G3 <- mean(E1$G3)\n",
    "sd_G3 <- sd(E1$G3)\n",
    "p1 <- ggplot(E1, aes(x = G3)) + \n",
    "  geom_histogram(aes(y = ..density..), bins = 30, color = \"black\", fill = \"blue\") + \n",
    "  stat_function(fun = dnorm, args = list(mean = mean_G3, sd = sd_G3), color = \"red\", size = 1) +\n",
    "  ggtitle(\"Histogram of students final grades in Portuguese language\")\n",
    "p1"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "id": "77bea1dc",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:55.935137Z",
     "iopub.status.busy": "2024-07-27T21:21:55.933382Z",
     "iopub.status.idle": "2024-07-27T21:21:56.100467Z",
     "shell.execute_reply": "2024-07-27T21:21:56.098256Z"
    },
    "papermill": {
     "duration": 0.182326,
     "end_time": "2024-07-27T21:21:56.104099",
     "exception": false,
     "start_time": "2024-07-27T21:21:55.921773",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "\tShapiro-Wilk normality test\n",
       "\n",
       "data:  resid(M1)\n",
       "W = 0.93482, p-value = 3.197e-16\n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "data": {
      "image/png": "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\nzZrVq1cbGhqePHly8ODBcr4xIyNj/vz5J0+elEql9ezWrFmzH3/8cdasWbUfHQsAAKB23vNI\nMY+lEdmD4/adiH9ZwbFz/mL6aNf/f0WS9neeTYe+s5cELBrXU9EpoTF78eLFuHHjEhMTnZyc\nwsPD5X9o7/3793v16vXmzRtLS8usrKwmTZqUlpYSQmQyGb0Dj8cjhFRXV1dVVc2ePTslJeWP\nP/5Q0LcAAABQgvc/K9bUof93Dv3fGqTY+tXiKqwYBop24cIFHx+fV69e+fn5rV+/nq5i8pBK\npRMnTiwrK/Pz8wsKCvL09ExMTGzWrJm+vv6DBw9kMtm3334bFBTk7Oz88uXLV69e9e7de+vW\nrR4eHl5eXgr9RgAAAIrz8d0MrQ4USiKRrFixwsPDo6KiIiwsLDg4WP5WRwhJSEi4c+fOggUL\nEhISjI2NnZ2dS0pK5syZc//+/VmzZgkEgjt37nz55ZdXr179/vvvKysrHR0ddXV1g4KCFPeN\nAAAAFA31DFRRfn6+p6fnypUrHRwcbt++PWbMmA89QmJiIiFkyJAhd+/e/eKLL65fv66jo0Of\nhJ06dWrv3r2TkpLow4pEImNj49u3b/ft2/fKlSsSiaTBvw4AAIByoNiByrl48WKnTp3Onj07\nc+bM5OTk1q1bf8RBXr9+TQjh8/mEEAsLi9evX5uYmNDrHZqbm1tYWAiFQnqVnNevX5ubmxcW\nFlpYWIjF4uLi4gb9NgAAAMqDYgcqRCaTrVu3rl+/fmVlZYcPH962bRvdzD6CkZERISQvL48Q\n8uTJEyMjo4KCAgMDA0LIy5cvc3Nz+Xw+3eGMjIxevnxpbGycm5vL4XCwJiIAAKiv9988AaAc\nBQUFvr6+Z86ccXBwiIiIsLW1/ZSj0QuXDBkyhBBy/PhxHR2d8vJy+jTrnj17Ll682KNHj/Dw\ncEIIl8stLCzs0qXLjh07unXrhgXtAABAfWHGDlTC5cuXHRwczpw54+vrm5SU9ImtbunSpcuX\nL2ez2SwWq1u3bjXja9assbOz27ZtW0VFhYODw549e9zc3AIDA7W1tW/fvl1WVrZgwYJP/ioA\nAACMQbEDhslksuDg4H79+hUXFx84cCA0NFQgEHzKAaOjo3/66afu3btfvHjRyMjoypUrVlZW\n5eXlWlpa1dXVDx8+pG+h2LBhA5vNvnXrVkZGhpaW1sWLF+fMmTNy5MgG+loAAAAMQLEDJhUW\nFg4dOnTBggWtW7e+cuWKj4/Ppx8zMDBQT0/v2LFj3bt3v3nz5ogRI3JycgghVVVVb+0pEolE\nIpFMJhMIBNu2bdu0adOnfzoAAACDcI0dMObmzZve3t7Pnz/39fX9448/dHR0Pv2Y5eXl165d\nGzNmjKmpKSHE2tr62LFjBQUFd+7cKSsrS0pK2rBhw/Tp0z08PKRSqUgkqnlWLIuFv+QAAIDa\nQ7EDBshkspCQkMWLF7NYrG3bts2cObOhjpyfny+VSq2trWsPmpiYeHh4EEJsbW03bNhgbW3t\n7e3dUJ8IAACgOlDsQNlKSkqmT58eERHRtm3b8PDwjh07NuDBmzRpQv5/Ebt3FRYWEkKwoAkA\nAGgqnH4Cpbp9+3bnzp0jIiJGjRp17dq1hm11hBAjIyNbW9vY2FiRSPTuqydPniSEuLi4NOyH\nAgAAqAgUO1Ce7du3d+vW7cWLF0FBQUePHlXQzNlXX32VlZX17bff0ne/1khISNi8eXPnzp1R\n7AAAQFPhVCwoQ2lp6cyZM48cOWJtbR0eHq7QajVv3rxTp05t2rTp9u3bM2bMsLOzy8/PP3Xq\n1O7duwUCwZ49e+i1iwEAADQPih0o3P3798eMGXP//v0RI0bs2bOHfq6X4vB4vFOnTi1ZsmTb\ntm3Jyck14926dduxY0f79u0V+ukAAAAMQrEDxQoNDZ09e3Z1dXVgYKC/v79yPlRbWzs4OHjZ\nsmXx8fHZ2dn6+vpdu3a1t7dXzqcDAAAwBcUOFKWysnLevHm7du1q0aJFWFiYq6urkgM0bdoU\ny5oAAECjgmIHCvHw4cMxY8akpqYOGzZs7969hoaGTCcCAADQfLgrFhpeaGiok5PTw4cPAwIC\njh8/jlYHAACgHJixg4ZUVVXl7+8fEhJiZWV15MiR7t27M50IAACgEUGxgwbz6NEjb2/ve/fu\n9e/f/8CBA2ZmZkwnAgAAaFxwKhYaRmRkZNeuXe/fvx8QEBAbG4tWBwAAoHyYsYNPJRQKFy9e\nHBISYmpqGhkZ2a9fP6YTAQAANFIodvBJMjIyvL29r1+/7u7ufujQIXNzc6YTAQAANF44FQsf\n7/jx4w4ODjdu3PD39z937hxaHQAAALNQ7OBjVFdXL1myZOTIkVwu98yZM4GBgWw2m+lQAAAA\njR1OxcIHy8zMHDt27NWrV3v37n3o0KFmzZoxnQgAAAAIwYwdfKiTJ086Ojpeu3bNz88vLi4O\nrQ4AAEB1oNiBvMRi8YoVK0aMGMFisU6dOhUcHMzhYMYXAABAheAXM8glOzt73LhxSUlJLi4u\nYWFhNjY2TCcCAACAt2HGDt4vLi7OyckpOTnZz88vMTERrQ4AAEA1odhBfSQSyYoVKwYOHFhV\nVRUREREcHMzlcpkOBQAAAHXDqVj4T3l5eT4+PvR0XXh4eMuWLZlOBAAAAPXBjB3ULT4+vlOn\nTnFxcTNnzkxKSkKrAwAAUH0odvA2mUy2bt06Dw+PioqK8PDwbdu28Xg8pkMBAADA++FULPxL\nfn7+xIkTz54927lz5/Dw8NatWzOdCAAAAOSFGTv4n4sXLzo4OJw9e9bX1zcxMRGtDgAAQL2g\n2AEhhMhksuDgYA8Pj9LS0sOHD4eGhmprazMdCgAAAD4MTsUCKSgomDRpUkxMTPv27SMiItq3\nb890IgAAAPgYmLFr7K5fv+7s7BwTE+Pr63vjxg20OgAAAPWFYtd40adfe/To8erVq507d4aG\nhgoEAqZDAQAAwMfDqdhGqri4eNq0aZGRkXZ2dhEREfb29kwnAgAAgE+FGbvG6ObNm46OjpGR\nkb6+vjdv3kSrAwAA0Awodo3O9u3bu3fvnpubGxQUFBoaqqOjw3QiAAAAaBg4FduIlJSUTJ8+\nPSIi4rPPPouIiOjYsSPTiQAAAKAhodg1Frdv3/b29n727JmXl9fu3bv19fWZTgQAAAANDKdi\nG4XQ0NAePXpkZ2cHBQVFRkai1QEAAGgkzNhpuNLS0pkzZx45csTa2jo8PNzFxYXpRAAAAKAo\nKHaa7P79+2PGjLl///6IESP27NljYGDAdCIAAABQIJyK1VihoaHOzs6PHz8ODAw8duwYWh0A\nAIDGw4ydBqqsrPTz89u5c2eLFi2OHDni5ubGdCIAAABQBhQ7TfPw4cMxY8akpqYOHTp07969\nRkZGTCcCAAAAJcGpWI2yf/9+Jyenhw8fBgQEHD9+HK0OAACgUcGMnYaoqqry9/cPCQmxsrI6\ncuRI9+7dmU4EAAAAyoZipwkeP348ZsyYe/fu9evX7+DBg2ZmZkwnAgAAAAbgVKzaO3bsmIuL\nS1paWkBAwNmzZ9HqAAAAGi3M2KkxoVC4ePHikJAQU1PTo0eP9u/fn+lEAAAAwCQUO3WVkZHh\n7e19/fp1d3f3Q4cOmZubM50IAAAAGIZTsWrpxIkTDg4ON27c8Pf3P3fuHFodAAAAEBQ7tSMW\ni5csWTJy5EgulxsTExMYGMhms5kOBQAAACoBp2LVSWZm5rhx465cudKrV6/Dhw83a9aM6UQA\nAACgQjBjpzZOnjzp6Oh49epVPz+/uLg4tDoAAAB4C4qdGhCLxStWrBgxYgSLxTp16lRwcDCX\ny2U6FAAAAKgcnIpVddnZ2ePGjUtKSnJ2dg4PD7exsWE6EQAAAKgozNiptPPnzzs5OSUnJ/v5\n+SUlJaHVAQAAQD1Q7FSURCJZsWLFgAEDqqqqIiIicPoVAAAA3gunYlVRXl6ej49PXFyck5NT\nWFhYq1atmE4EAAAAagAzdionPj7ewcEhLi5u5syZSUlJaHUAAAAgJxQ7FSKTydatW+fh4VFe\nXh4WFrZt2zYej8d0KAAAAFAbOBWrKvLz8319fWNjYx0dHcPDw21tbZlOBAAAAGoGM3Yq4dKl\nSw4ODrGxsb6+vklJSWh1AAAA8BFQ7Bgmk8mCg4P79+9fWlp66NCh0NBQbW1tpkMBAACAWsKp\nWCYVFBRMmjQpJiamXbt2ERERn3/+OdOJAAAAQI1hxo4xN27ccHZ2jomJ8fX1vXnzJlodAAAA\nfCIUOwbQp1+7d+/+6tWrHTt2hIaGCgQCpkMBAACA2sOpWGUrLi6eNm1aZGSknZ1deHh4hw4d\nmE4EAAAAGgIzdkp169atzp07R0ZGTpw48ebNm2h1AAAA0IBQ7JRn+/bt3bp1y8nJCQoK2r9/\nv46ODtOJAAAAQKPgVKwylJSUzJgxIzw8/LPPPgsPD+/UqRPTiQAAAEADodgp3J07d7y9vZ8+\nferl5bV79259fX2mEwEAAIBmwqlYxQoNDe3evXtWVlZQUFBkZCRaHQAAACgOZuwUpbS0dNas\nWYcPH7a2tg4LC+vatSvTiQAAAEDDodgpxIMHD8aMGZOWljZ8+PA9e/YYGhoynQgAAAA0H4pd\nwwsNDZ09e7ZIJAoMDFy8eDFFUUwnahSuX79+4MCBlJSU/Px8kUjE4XBMTEzatWs3duzY/v37\nM50OAABAGVDsGlJlZaWfn9/OnTubN28eFhbm5ubGdKJGQSwWz5kzZ8eOHYQQHo8nEono8b//\n/jspKWnnzp3Dhg07ePCgrq4uozEBAAAUDjdPNJiHDx927dp1586dQ4YMuXv3Llqd0ixYsGDH\njh2DBg1ydXUViURTp069devW4sWLJRLJZ599Nn78+Ojo6PHjxzMdEwAAQOEwY9cwDhw48NVX\nXwmFwoCAgOXLl7NYaMxKkpaW9scff/Tr12/69OmjR4+eNWvW1q1bCSGdO3c2MDD44YcfZs+e\nzePx9u3bd+bMmUGDBjGdFwAAQIHQPz5VVVXV/PnzfX19DQ0NExISVqxYgVanTEePHpVKpStX\nrjx69CibzV61alXNS4sWLTI0NAwPD1+9ejVFUeHh4QzmBAAAUALM2H2Sx48fe3t7p6Sk9OvX\n7+DBg2ZmZkwnanSePHlCUZSzs/OTJ0+sra1NTU1rXuJyuY6Ojvfu3WvevLmFhcWTJ08YzAkA\nAKAEmFv6eFFRUS4uLqmpqQEBAWfPnkWrY4RMJiOEUBQlk8nenSulx+kNqVTKQD4AAAAlQrH7\nGEKhcP78+V5eXnw+PyYmBqdfGWRrayuTyW7fvm1ra5uenl5YWFjzklgsTklJad26dW5ubm5u\nrq2tLYM5AQAAlAB15INlZGT07t07JCSkT58+d+/e9fDwYDpRo+bl5UVR1KpVq7y8vMRi8Zo1\na2pe2rRpU0FBwahRo1auXCmVSkeNGsVgTgAAACXANXYf5sSJE1OnTi0qKvL391+7di2bzWY6\nUWPXqVOnL7/8cteuXXw+38nJKSgoSCKRzJ8//9ChQ6tWrWrVqtXjx4937drVv3//oUOHMh0W\nAABAsVDs5CWVSpcsWfLLL7+YmJjExMQMHDiQ6UTwjy1btlRUVBw+fJgQoqWltXHjxo0bNxJC\n+Hx+enr6rl27+vXrFxERgUeAAACAxkOxk9fOnTuzsrJ69ep1+PDhZs2aMR0H/ofH4x06dGjG\njBn79++/e/duQUGBRCJhsViGhob29vZjx44dOnQoLoIEAIDGAMVOXtnZ2T/88MPKlSs5HPxL\nU0Xu7u7u7u5MpwAAAGASOoq8Jk6cuHbtWqZTAMnLyyspKTEwMNDT08vKyuJyuRYWFrm5udXV\n1VZWVlpaWkwHBAAAYAyKnbw+++wzpiM0alKpdNu2bSEhIQ8fPqRHaq9RR2/w+fxBgwatXLmy\nU6dOTGYFAABgCC48AjUgFotHjRo1Z86cN2/eeHh4cDgciqJqbknW1tYmhOjo6Dg7O588ebJr\n165RUVGM5gUAAGAGih2ogbVr1x4/ftzHxycpKenKlSvGxsbJyclt2rShb4n46aefIiIixGJx\nXl4e/aqPj096ejrTqQEAAJQNxQ5UXVVV1YYNG+zt7fft27d9+/aysrK9e/e+ePHiwYMHy5Yt\ns7GxCQwM9PLyWrVq1ePHj58+fXrw4MHKysqgoCCmgwMAACgbih2ouuvXr5eWlk6ePJnNZsfF\nxVlaWg4cODAuLo4QMmPGjAkTJrx8+TItLe3LL78khMTFxfXp06dVq1b0DgAAAI0Kih2oupcv\nXxJCWrZsSW/b2NhQFPXy5Usej2dlZUWP5+bmmpiY6Onp1exMbwAAADQqKHag6vT09Aghb968\nIYQ0adKkqKiI3hCJROXl5TXjQqGwoqKiZucmTZowmhoAAIABKHag6jp37sxms0+dOkUIcXZ2\nvn///t9//+3k5EQI+fPPP0+dOqWlpWVvb3/mzBmJROLs7JydnZ2SkkLvAAAA0Kig2IGqMzMz\nGzFixIkTJ44cOTJz5kxCyLRp04YPH66vrz9nzpyLFy/6+PiUlpYuXLhQV1d31KhR06dPl0gk\n9J4AAACNCoodqIGgoCBLS8sJEyZs37592LBhCQkJbm5uhoaGr1+/ZrFYT58+bd++/fPnz728\nvAYPHhwbGztnzpz+/fsznRoAAEDZ8OQJUANWVlZJSUmzZs3av38/PZKTk0NvSKXSixcv0tuh\noaFNmjQJDAz87rvvmAkKAADAKBQ7UA8tWrSIiYl59OjRpUuXXr9+raenx2azy8rKKIqSSqX0\nU8VsbW379u2L2yYAAKDRQrEDddK2bdu2bdsynQIAAEBF4Ro7AAAAAA2BYgcAAACgIVDsAAAA\nADQEih0AAACAhkCxAwAAANAQKHYAAAAAGkJdlzt5k/v80aMnr16XlFdUcbR09I3N29i1a2Vh\nwHQuAAAAAMaoWbGTSYrDf18ZsutQ8sNX775qbuc6Yfr8H+ePNeBQys8GAAAAwCx1KnYS0Yup\nzp323ytkc4269h3WsV1rCxMDPp8jFgqLCl5mPElLvnztt0XjQw/9mXIltBkPZ5kBAACgcVGn\nYnfl20H77xX2mBt8OHCOlU4dyaWiwsPrvvYNOOQxb9VHTmgAACAASURBVHratj5KDwgAAADA\nJHWa1vph/xNdi68ub/Srs9URQlg8Y58fj/zR1ezZkWVKzgYAAADAOHUqdn+VV+u2GPre3br0\nMq2uSFNCHgAAAACVok7Fbrix9puHgS9F0vp2klbuDk/XMhyorFAAAAAAqkKdit3SdQOFxZft\nXb0PxN4ql8jeflkmvH85arpHuz/SS/oEBDAREAAAAIBJ6nTzRJvJETtuDJi15ZjvoEg2T79V\nm9bNmhrw+VyJSFhckPv3k2evq8QURbnP2Rz9dTumwwIAAAAomzoVO0JY0zfFfeF7fPOew6fj\nrz58cOdJ2j/zdhSLb9X6cw/3geOn+w13tmQ2JQAAAAAj1KvYEUKIZdcRP3Ud8RMhMnFlUVFp\neaWIpy3QMzDUxqLEAAAA0LipX7GrQXG0DU20DZmOAQAAAKAi1OnmCQAAAACohxrP2DUIiURy\n+vTpqqqqevZJT08nhEil9S6zAgAAAMC0xl7s4uPjhw0bJs+e2dnZig4DAAAA8CnUqdgVvcwt\nl8g7bWZpKde9se7u7tHR0fXP2J06dWrfvn0TJkyQ86MBAAAAGKFOxe47x892viyTc2eZ7J0V\njOvCZrOHDn3PY8pycnL27dvH5XLl/GgAAAAARqhTsVsTF9N27+blv4dVSmSGHfp0t9ZlOhEA\nAACAClGnYmf2eY9F63u4G/3t9MP1dl//cXKWHdOJAAAAAFSI+i130uHrDUxHAAAAAFBF6lfs\neE16dLYy19diMx0EAAAAQLWo06nYGreycpmOAAAAAKBy1G/GDgAAAADqpAnF7tmhSY6Ojkyn\nAAAAAGCYJhS7qvzHd+/eZToFAAAAAMM0odgBAAAAAEGxAwAAANAYKHYAAAAAGkITil272eeL\nioqYTgEAAADAMLVcx+4tLJ6OPo/pEAAAAABM04QZOwAAAAAgKHYAAAAAGgPFDtRDcXGxUChk\nOgUAAIBKQ7EDlZaWlubj42NoaGhgYKCtrd2xY8fff/+9urqa6VwAAACqSBNungBNFRYWNmXK\nFKFQ6Obm1q5du7Kysvj4+IULF4aHh585c0ZfX5/pgAAAAKoFxQ5UVFpa2qRJk8zNzY8ePers\n7EwPCoXCgICAdevWTZs27ejRo8wmBAAAUDUodqCifvrpJ7FYHBUV1blz55pBPp8fGBiYnp4e\nFhZ27969jh07MpgQAABA1eAaO1BRMTExrq6utVtdjTlz5hBCYmNjlR4KAABApaHYgSqqqKh4\n8+ZNmzZt6nyVHs/OzlZuKAAAAFWHYgeqiM/nczicsrKyOl+lx3V0dJQbCgAAQNWh2IEqYrPZ\nHTp0uHTpUlVV1buvnjt3jhDi4OCg9FwAAAAqDcUOVNSXX36Zn5///fffvzWekZGxatUqMzMz\nT09PRoIBAACoLBQ7UFGzZs3q1atXUFDQ4MGDz5w58+LFi7S0tN9++83JyamgoGDbtm26urpM\nZwQAAFAtWO4EVBSXy/3zzz/nzp174MCB06dP14xbWFhERUUNHTqUwWwAAACqCcUOVJeent6+\nffuWLVt2+vTpjIwMHR2dzp07e3p68vl8pqMBAACoIhQ7UHVt2rSZP38+0ykAAADUAK6xAwAA\nANAQKHYAAAAAGgLFDgAAAEBDoNgBAAAAaAgUOwAAAAANgWIHAAAAoCFQ7AAAAAA0BIodAAAA\ngIZAsQMAAADQECh2AAAAABoCxQ4AAABAQ6DYAQAAAGgIFDsAAAAADYFiBwAAAKAhUOwAAAAA\nNASKHTSM8vLydwcrKipkMlntEZFIVF1dXc8ba//47tsBAACgHih28PFkMtnBgwd79uzJ5XJ1\ndXX19fXHjBlz48aN1NTUiRMnGhsb6+jocLlcFxeXoKAgf3//1q1b8/l8Ho9na2vr7e3t7u6u\nra2tq6uro6Pj4OBgb29PH6dJkyYtW7Y0NDSk3+7k5LR161aJRML01wUAAFB1HKYDgLqqrq4e\nP358ZGSknp7egAEDTE1Nnz59euzYsaioKIqiJBKJi4uLnZ3dmzdvEhISvvnmG0JIy5Ytx48f\nTwg5depUREQERVFubm62trYxMTEpKSmEEFtbWysrq0uXLpWWllIU5erqamZmdvny5dmzZ4eF\nhf355586OjoMf20AAAAVhhk7+EjLli2LjIwcN25cZmbmqVOn9uzZc/ny5fDwcKlUKhaLN2/e\nfPXq1b179x45csTIyIjNZhNC3N3dDx065ObmVlJS8vnnn3M4nLy8PENDw/z8/BEjRri6uj59\n+jQxMdHS0vLgwYN2dnbXr1//5ptvsrKy5s2bl5CQ8NVXXzH9pQEAAFSbDN4nKCiIEJKYmMh0\nEBVSWFjI4/G6du0qFotrj48cOZLD4WhpafXo0YMe2bJlCyEkKCjoiy++YLFYjx8/NjExadmy\nZUVFxYYNGwghbDa7W7duEonkzZs3WlpahJBbt27JZLK///5bW1u7T58+9HGGDRtGUdTDhw+V\n/E0BAADekpiYSP9qYzpIHTBjBx/j/PnzIpFo9uzZ9FQcTSqVnjlzpnfv3sOHD09OTi4uLiaE\nnD59WktLa8aMGV9//bVUKt2+fXtBQcG0adO0tbVnzpzJ4XAkEsmcOXNYLJaBgYFUKiWEaGtr\nE0Jatmzp6el5+fLlsrIyQsjcuXNlMtmZM2cY+sYAAABqAMUOPkZ2djYhpG3btrUHX79+XVlZ\n2bZtWzs7O6lUmpOTQ+/ZvHlzgUBA7/zs2bOaN9L3SdT8WFJSIhKJag5OCLGzs5NIJLm5uTX7\nZGVlKe9LAgAAqBsUO/gYAoGAvLNSSc0gPcdG/ygQCOjd6H/Sdz/UvJFe+oT+UUtLi6KomjcS\nQmofh96n5iUAAAB4F4odfIxOnToRQuLi4moPCgQCW1vbhISEuLg4Y2NjKysres+cnJz79+/T\nO/ft25eiKHr73r17paWlNcfh8Xg6OjoURbVp04YQIpPJzp8/b2pqamFhUbMP/bkAAABQJxQ7\n+BguLi7t27fftGnTvXv3ao9PnTo1IyMjJSXF19eXvvxuypQpFEVNmzYtMDDQyspq/Pjx/fr1\nO3z4cGxs7Lx581gslrW1dXBwcGpq6okTJ8rLy2UyWXBwMCFk48aNqampkyZNYrFYGRkZa9eu\nNTc3HzRoEDNfGAAAQB2g2MHHYLFY27dvF4vFPXr0+Pnnn9PS0goKCq5cufLo0SN6h4yMjIsX\nL+bl5RkYGPTs2fPq1auFhYW+vr6FhYVLly7V0tL64osvLl26NHHixE2bNgmFwi5dunh5eRkZ\nGXXo0OGnn36ysbFZsGBBmzZtxo8fHxIS4uLiUlBQsHXrVqxjBwAAUA9Khkc2vU9wcPCCBQsS\nExO7d+/OdBbVcunSpSlTpjx//rz2YJ8+fQQCwenTp2sP6urqCoXC2g8To6gP+P+emZnZtm3b\nhg8f/umZAQAAPlFSUlKPHj2CgoLmz5/PdJa34ckT8PF69er16NGjM2fOXLlypaioyNLScsCA\nAc7OzoSQe/fuxcTEZGZm6unpOTk5DRky5M2bN9HR0Q8ePKAoql27dkOGDElLS0tMTMzLyzM3\nN+/Ro0dpaen169eLi4stLS1tbW3T09MzMzN1dHS6dOkydOhQeg0UAAAAqAeKHXwSLpc7dOjQ\noUOHvjXesWPHjh071h6xsLCYNWtW7ZFmzZp5eHjUHhkxYoSCcgIAADQGuMYOAAAAQEOg2AEA\nAABoCBQ7AAAAAA2BYgcAAACgIVDsAAAAADQEih0AAACAhkCxAwAAANAQKHYAAAAAGgILFCtV\ndXX1ixcvCCGWlpZcLldBn1JZWZmTk8Pj8aysrCiKkuctZWVlr1694vP5VlZW/7XPy5cvy8vL\nTUxM9PX1Gy4sAAAANBjM2CnJ8+fPJ02aZGRk1LJly5YtWxoZGU2aNOmtp6x+uuvXr3t6ehoY\nGNja2rZo0cLU1PTbb78tKiqq5y0JCQn9+vUzNDS0tbVt3rx5s2bNfvzxx4qKipodxGLxr7/+\n2qpVKwsLC1tbWyMjo65du0ZFRTVscgAAAPh0mLFThuTkZE9Pz+Li4p49e7q6uhJCrly5sn//\n/ujo6JiYGDc3twb5lNDQ0GnTphFCBg4c2LFjx4qKivPnz//222/Hjx9PSEho3rz5u28JDg5e\nuHAhl8sdNmxY27ZtS0tLY2Nj16xZc+LEiQsXLpiYmFRUVHh6el68eNHKymrWrFlGRkYZGRnR\n0dFeXl6LFi1av359gyQHAACAhiGD9wkKCiKEJCYmftzbi4uLzczM9PT0zp07V3v83Llzenp6\nZmZmxcXFnx4yNTWVy+W2atUqLS2t9vjWrVtZLJabm5tUKn3rLYmJiRRF2dvbP3/+vGZQIpH8\n/PPPhJAhQ4bIZLLZs2cTQubNmycSiWr2yfu/9u47Poo6/+P4Z7ZmN4UUktATIFJCBCKIofgj\nAiKCBTwQEJB+gEgTG4InKhwgnggeqEQQxXaIB0rTo0sT0TtQ4UBKKEovaaTu7vz+WFlCQrK7\nJGSXudfzDx/Z78x857OfXWfeZHcmZ8+2bdtWRD777LOyVw4AwK1l69atIvLmm2/6upDr4KPY\nm27BggVnzpx56623OnToUHi8Q4cOc+bMOXPmzIIFC8q+l5kzZ9pstqVLl8bHxxceHzZs2Jgx\nY3bs2LFx48Yim0ybNs1oNH755ZexsbGuQZ1O9/zzz/fr12/lypWbN29OSUlJTk6ePXt24W8E\nRkZGLlu2LDw8fOrUqWWvHAAAlBeC3U23du1aq9Xaq1ev4ot69+5ttVrXrVtX9r2sW7cuMTEx\nMTGx+KJBgwY5Vyg8qKrq+vXrk5OT69SpU9ImCxcutNlsAwcOLH4FRlhYWLdu3X7++eczZ86U\nvXgAAFAuCHY33enTp6tVq2Y2m4svMpvN1apVO336dBl3oarq6dOna9eufd2lzvFTp04VHkxL\nS8vNzS19k99++01Erpv8XONlLx4AAJQXgt1NFxISUsp1qWlpacHBwWXchaIowcHBJe3l0qVL\nzjIKDwYFBSmKUvomztuaOH/2cFoAAOBDBLubrnnz5ufPn9+5c2fxRd9999358+ebN29eLnvZ\nsWPHxYsXiy9avXq1c4XCg0ajsUmTJhs2bMjJySlpk44dO4rIqlWriq/gcDjWrFkTFRVVq1at\nshcPAADKBcHuphs0aJDBYBgxYkSRX4+lpaU98cQTBoPB+YW2Mho2bFh2dvawYcMKCgoKjx86\ndOjFF1+Mjo7u2rVr8U3OnTs3ZswYh8NReHz37t0zZsyoW7fuoEGDWrduvXDhwrVr1xbZdsqU\nKXv37h08eLBery978QAAoFxwH7ubLj4+/tVXX50wYUKTJk3Gjh3rvGvdjh07Zs2adeLEienT\npxe5jvXGdO/evVevXp999tnhw4dHjRqVkJCQk5Ozbt26OXPmXL58edmyZcU/8B06dOjy5ctT\nUlL27t07YsSIBg0aZGVlrV69eu7cuQ6HY9GiRSaTKSUlpXXr1p07dx46dGjXrl0jIiJSU1MX\nLFjw9ddfN2vWbOLEiWWvHAAAlBtf32/lFlDG+9g5paSkREZGFu58ZGTke++9V15Fqqqan58/\nadIkq9VaeC9xcXFF7p9XWHZ29pgxY0wmU+FNEhISduzY4Vpn3759rVq1KryCTqd7/PHHL126\nVI7FAwBwq/Dn+9gpqqrerMyoFbNnzx47duzWrVtbt25dlnlycnI2bNiwf/9+RVHq16/frl07\ni8VSXkW6XLx4cf369ampqWazOTExsXXr1m4/LT179uz69euPHz9utVqbN2+elJRU/P4mu3fv\n3r59e2ZmZtWqVZOTk/lqHQDgf9a2bdvatGnz5ptvjhkzxte1FMVHsRXHYrF06dKlS5cuN3Uv\n4eHhPXr08GqTqKio3r17l75O06ZNmzZtWoa6AADATcfFEwAAABpBsAMAANAIgh0AAIBGEOwA\nAAA0gmAHAACgEQQ7AAAAjSDYAQAAaATBDgAAQCMIdgAAABpBsAMAANAIgh0AAIBGEOwAAAA0\ngmAHAACgEQQ7AAAAjTD4uoBbxoEDBwICAip+vwUFBYsWLYqJidHpSOHuORyOQ4cOxcXF0S5P\n0C5v0TGv0C6v0C6vOByOY8eODRgwwGg0VvzeDxw4UPE79RDBzj3nm2bw4MG+LgQAAFz17rvv\n+nDvPsmUbhHs3OvTp4/NZsvJyfHJ3n/66adPPvmkTZs2MTExPing1nLs2LGtW7fSLg/RLm/R\nMa/QLq/QLq842/XYY481btzYJwVYLJY+ffr4ZNduqPBvS5YsEZElS5b4upBbA+3yCu3yFh3z\nCu3yCu3yCu0qCR/kAwAAaATBDgAAQCMIdgAAABpBsAMAANAIgh0AAIBGEOwAAAA0gmAHAACg\nEQQ7AAAAjSDYAQAAaATBzt9ZLBbXf+EW7fIK7fIWHfMK7fIK7fIK7SqJoqqqr2tAaex2+/r1\n69u3b6/X631dyy2AdnmFdnmLjnmFdnmFdnmFdpWEYAcAAKARfBQLAACgEQQ7AAAAjSDYAQAA\naATBDgAAQCMIdgAAABpBsAMAANAIgh0AAIBGEOwAAAA0gmAHAACgEQQ7AAAAjSDYAQAAaATB\nDgAAQCMIdgAAABpBsAMAANAIgh0AAIBGEOwAAAA0gmB3a7j8+7fj+3SuWyXcbDSHV4nr3Oep\nTcezfF2U/3IUnHt74vAW9WMrWU2BoZF3tuuR8s0hXxd1C8g++2FiYuKeywW+LsQ/OdbOn5jc\nuHawOSCqZvzjT88+me/wdUm3AN5UnuCQ5RVOiKVTVFX1dQ1wI/fChia1Oh3MscXf/WBSg8qn\n9m9f/e1+Q0DsF0f3PhRt9XV1fsdhOz+oSb0P9l0KjrnzofZNs3/bt3rd9nxV13/+7veHJPi6\nOr+2ZnjDzu/u356R1zLY5Ota/M7nT7Z4dO6uwGqJD7RvdHHf5rU/nghPeDx196IQveLr0vwa\nbyq3OGR5hROieyr83tIuMSLSb+GPrpFtczqLSLW2H/uwKr+1Z1qSiNR6cFqmzeEcObPrk+pm\nvd4UvfdygW9r81tZZw59+sZIg6KIyPaMPF+X43cyjs7TK0pInf4n8+zOkcXDG4lI8qxffFuY\nP+NN5SEOWV7hhOgWwe4W0CTIZApuZi88ZM+OMOrNldr4qiR/Nr5GsKLot6VfcyLZOjJeRLp+\ne9JXVfmz5Frhhf+xxzm4uH/1qCMiT+057xqx5aaGG3WWyt18WJU/403lOQ5ZXuGE6Jbh5v9O\nEGWj5tdM7lgv4qFrvg6pM5t1clnho43r2JiWZwpu0SrkmuZU71BF5u47dyBD7q7qq8L8Vv/x\nLz5QYBeR72f8Zcm5bF+X44/mbTylM4RObnQ1rOjNsc/VCnnu8LJdWQV3Bhl9WJt/4k3lOQ5Z\nXuCE6AGCnd9TTCtWrCgytufT4Sfz7HHdn/JJRX7ug227VENYkcE9H6aKSL07I3xRkb8bMHqs\n84f35/+Vc3BxqiN7zcXcgMoPB1/7dbq7mkXI4bRl53MIdsXxpvIchywvcEL0AMHuVnJi1eRn\nF+05cWjPtt2pTR8au3rh/b6uyB8lNG5cZOT0tll9vzpmDmn1RiOOkvCaPe94nkOtZC36NfaQ\n+BAROZjN9Z4oEw5ZN4YTYkm43cmtJOfU3t0//3Lw0G+KotMVXD58Mc/XFfk71Z7+0dTBt7V9\nOkcXMXP9l6EGLmCE1xwF50VEpw8pMm4MMopIdjrBDuWGQ5bnOCGWhN/Y+RHVnv7a6++4HpqC\nmowb2anwCvWGfP7fISJq/paPX+7Yf9p9TU9eOrXS9L/6P77bdv36zTtDhz/77dHMsAb3LfzH\nJz0ahxeb43+I23ahJDpDmIg47JlFxguyCkTEHMxRFOWDQ5ZXOCGWyNdXb+AqW25q4ZcmqMqQ\nUlb+tG11EXn5WHqFledvSmmXveDCa4PaiIgxqM742cuy7Q4f1uknPHl3LawXLlzAWIzDnhWg\nUwKj+xUZ39wrTkSeOZLmk6puFbypPMEhq4w4IRbGR7F+RG+OLfzaZJ5KEZGs32d369Zt3OLD\nRVau3zZKRHan5/ugUP9w3XaJiOq4PL5dwrMLtzbu/sIvp/a/PrqrRcc/4kpsF9xSdIH3hQXk\nXvw699q/NLHnxwsi8khli2/KglZwyPIcJ0RPEOz8nc5Yefny5Z/M2lZk/PCWsyLSLNTsi6L8\n2u7p97255VTi6E/2fD61HpcrojyMbFvFXnDutSNprhFHwfkZxzMslbsm8QcVUDYcsjzHCdET\nBDt/Z4167IEIy/mfnlzw43nX4NnvUwZvOWWu1GZc9SAf1uaX7H+evssY2GjD33r5uhJoR9Lf\nnlEU5e+9XnP90m7La4/8nmdvMWmKT+uCBnDI8gInRE/wtV//pyxY9VKd1i8MbVFr4X1dGlYP\nPHlo34ZvfyjQhU5b8bmV39hfK/fimh8y8w0B2d3ubVd8adK8f05rWPR+UYBbwbEjPh32bq93\nptVtvb9/x9sv7tsw/4ttYQ0HLBsZ7+vScGvjkOUlTojuEexuAVF3PXd0V50JU+at2bph17+y\ngirXaNdz9OgXJ3dqGOrr0vxOXtoGEbHlpm7alFp8aWAG38DADer59o+W256d+s6S2dNWWSrX\n7Pnk9NdffzqMu1GgbDhkeYsToluKqqq+rgEAAADlgO/YAQAAaATBDgAAQCMIdgAAABpBsAMA\nANAIgh0AAIBGEOwAAAA0gmAHAACgEQQ7AAAAjSDYAQAAaATBDgAAQCMIdgAAABpBsAMAANAI\ngh0AAIBGEOwAAAA0gmAHAACgEQQ7AAAAjSDYAQAAaATBDgAAQCMIdgAAABpBsAMAANAIgh0A\nAIBGEOwAAAA0gmAHAACgEQQ7AAAAjSDYAQAAaATBDgAAQCMIdgAAABpBsAMAANAIgh0AAIBG\nEOwAAAA0gmAHAACgEQQ7AAAAjSDYAbjGt71vU9xZfSnXudrG9Dxf1+uRlYnRiqIczbOXy2zr\n7o9RFGVHZn7pq13Y+6+JI/veUb92eHCAJSS8TsM7+4/+y6aDGeVSQ9kV6YmHTwqAnzP4ugAA\n/iWi+b1dcxNcD09vXPNdel79jg80tF49XEQb9Zd9UZuHMo5Nimk6t+W8H1b3ruubClTbRxMe\nGTRzZYFDDarWoFlSO4uafeSXXR++9cPiuTO7/2XxZy91r+B/Vfu+JwAqBMEOwDUajZ+3bPzV\nhysTox/cfbb7/I+nxIQUXu3biq7LC6ojNy0tLSvf4Rq5Z/n2/bm2GiZ9xRSwfGxSvzk/Wqvc\nPW/RvEH3JVzJcI7dK98dNuCpzyf3+D33m23TOlZMMU4+7wmAisFHsQBuGdkXbvCDwsCYuvXr\n1zco5VvO9V3aP/2Rt/5tDrlr24F1Q66mOhHRNX1gxOYDG28PNO6Y0Xle6s36TNaRVyi+lawi\newKgwhDsANw41VGwdPrIO2pXtZqsNW+7ve8zczPs6tWl9vSPp41uFR8TYjFH1Yy7t+/4f+1P\nLzJDQdaBGaMeS4ipYjGaI6rU7txn3KYjma6lG7vV0emtIrL0lUE1Kwfe8fQutzO/fVt4aJ2/\niciWAfUURZl76rKIrGlZrfD3yfLT970yrEe96pFmU2CNuGbDJs0/V3BNFso6tunpfg/Wrx4Z\nYDQGVYq6o23X2ct+8bAnH/eZpapq739+0TTEVHxpQETSVx89rKr2Kf2WOUeWN4pUFCW9UN9E\npF90kCWsg+clOb/yaMv5ddyDLazWAIM+oOZtt/d79m3ny+FJT4pw+9ptWfzX+5MSwoItJktQ\nXJO7J/x9lXrdiQBUMBUASraiaZSITDyaXmR8c684ERnco4ExqN6jg0Y9M2pwfESAiMQP+dq5\ngsOe9WSbKiIS3rBlrwGDH763lVmn6E3Rr2865Zqk4PJPyVUDRaRG41a9B/a/t1VjvaIYAmp9\ncOiP3W3oWlvRWb6bdq8puHb3ASNm/CPV7cx7P31/1pQOIhLX/5V33nnnl8sFqqquTqoqIqm5\nNlVV8zJ2tom0KIouoeW9Awf3u6dxlIhE3jk81/FHVdlnv4oNMCiKsXmnRwYP+3Ovbu3DDDpF\n0T2//bRzhbWdaonI9oy84u1y2NIrG/UGcy3XbNdbJy3CqDeYa+XYVVVVl8VXFpE02zUb9I0K\nDAht73rotiTnyzG+RZQx6LYeA598ZtSQRmFmEYkfvNqTnhR5Um5fu51T7xMRS1Sjnv0GD+nX\ns364WUQ6TPt3ic8ZQEUh2AEoTenBLiC83Xdnc5wj+Zl7apoNpuDmzod7prcRkWbjPsy7klhO\n7/yomllvCkq8UPDH0NKusSLScerXrmkPfjVJpyghMUOdDzd0ra0o+spVOv+Sme9ax+3MaUfG\ni8jdi351bVI4xMxvV11ERv9j75WFtvm964pI77UnnI93jkkQkV4fH3Btfn736yJSve0fdZYS\n7HLOLxORSrGvlN7VcTWCRWRTWp7qWbBzW5Lz5bBEtN955eXITdsWbdIbA293Piy9J0WelLsO\nO+oEGEzBzV3b5mX8EG7UBYR1KP1ZA6gAfBQL4MYlv7fgrsgA58/GoMYDoq32vN+dD0dP32UO\nab1xZl/TlW9xRbfos2RI/fys/0w/li4iqj196MrjAeGdVk24zzVh3IOvzk6MzDiW8tm5HOeI\nqtpbpMxrFGR0reN25lLYsveN2nQytO4Lsx+NvzKm7/fWzKSkJNu2C87H1e99cdGiRX9/NM61\nVWiDHiKSd6Wk0ubPTRURQ4CbK0/rBBhEJDXX5nZCr0pqt3BBiysvh7lSq6FVAu15v3m4i8JK\n77DqyD6eZ9cbo8MNf5xBTMHNvt/1w7Z1f7uBfQEoX1wVC+DG9WkdVfhhgO6PIFCQ9ePmtLyg\nqg2XLFpYeIW0QJ2IfP/DBakbmn1uySWbI6bl+CLf3+84qp4MPPvxofRekRbnSI87I11LPZm5\nlIKzTs7Nc6jxfbtfU3ZEtx07urkeVu/yaH8RQdGiigAABkBJREFU1Z6d+t9fjxw9evTI4S0r\n5pXeBxdDQKxciXelOJprE5G6Fk+PwB6W1DMpsvBDV/DyitsOK3XrTr+n2tMbVtWsf/eAxx5u\n27pVUssWdZsk3sC+AJQ7gh2AG1ethJtl2HJ+FZGsU+8NGfJe8aU5J3NExJ53TESCbwspsjSk\nYYiIZJ3IlpZ/jNQ0X92LJzOXIu/SMdcuSmLL3j95xOh5n224lG9XdMYqMXFN70wWOVL6zE7m\nSveEG3UZp9/LVyeaSrjgVLVnfngm2xAQ2yL4OldXlKWkCGM5fAjjSYef+uan8BmT3/lgyZxX\nn50jouhMtyd3e+G1t3o2iyy+CYCKxEexAG6cUkJ20Zuqi0iVFl9d9ysgO8cliIjeHCMimQcz\ni2ybdShLRKzVLK4RneLdzKUwhoSLSPbx7FLWmdiyzdQP194z9vWtew5l5eWdPLJv1SdvlD6t\ni2IIfSkhwpZ79Mktp4osyji4/Of0fBE5vmrouQJ7dNIUc8m3Gsm0X3OVbllK8pYnHVYM4QMn\nztn56+m0E/9d+WnK2Mc7Ht78eZ9WCVsy+MMVgI8R7ACUP1OlNvFWY8aRRUVuqHZo8dRx48Zt\ny8gXEWvlHqEG3dkds4rcb2P9WwdEpGe9Sjc8cymCqgxRFOXIB18XHszP3KHX6aKafCwituy9\nr/10IbTuzC9mjG3duK7VoIiIo+Cc26fs0u+jsSKyuGvPvZcLro6qtuFt+7aok/TuN6u69Vmm\nKMYpix8uvFW67eoTsuceWZt29W+1lb0kr7jtcO6FLydMmPDGF8dEpFKNBl16DXnj/RXfvpxo\nzz87fe/Fm1QVAA8R7ADcDLq3B9XPPv/PTi9/5coHmakr7x82+e2FO5sGGUVEMYTOv79mzsVV\nD8/c6NrsyOrJI78/G1JryONR1hue2clhu85tek2V/u+lhPCL+56buOLwlTF16bjBDlW9a1JL\nERHFoFMUW/ZB25XbsjkKzv195CMiIuLRn5oNi3/hiyea5l7aktTg/sWbDv4xqhje+3FVgn3f\n8E4P/Ccrv90r6wbUCHIusUSZRWTqhpNXysl/f/RD2YV/Y1fmklyu25Ni3HZYnT59+l9GTbpw\ndTb1+/9cFJHboy3XnRFAxSm362sBaFHptzvZkJZbeHBqbCW9qarzZ3ve73+qHyoikfWa9Rg4\nvF+PTqEGnU4f9OrGk67187N2/1+0VURimyf3Hza0S3IzvaIYAmI/Onz1PnbF9+J25owTr4lI\naL1HJr/80rb0PPXaW3tkn1ndKNikKPpmyV2Gjhjc6a4aIhKeMDDzyg1HprWpIiK123R/btJL\no4f2vSPaWqVFr5pmgzGw0V/ffFct9XYnf3AULHq6s0FRRCQ0tnGH+x964L72jaoHiYiiKCKS\n8OjkM/l257qntz2tKIrOEPLIkNGTnhnZqXm0ouibBZsK3+7EbUnOl2PVxZzCVbxRJ1RnCPOk\nJ0WelNsO//WeaiISWL3pnx4b9MTQAfckRItIdKunCkq+ex+AikGwA1CaGw52qqra8k689dzA\nxDpVLUZjVK169zw85IsfzxaZJz9j39SRPeNrRgYYjKGRMZ16j910JNO19LrBzv3M9pyJPVqG\nWo0ma9gHZy6rxULM5ZPfPdvvgdrRYUaDOTImod8zs07l2a9Onnvs1WEP144KMVnCGie1H/3a\n0jyHun7iI6EWY3CVO1RPgp2qqqp67qc1zw/v3SSuVqVAk8FsrR7X+LGx07YfuvTVtD5GRYls\n/rxrze8+mHx3k/phVoOI6AyhT8zeuiy+cuFg57Ykt8HObU+KPKnSO2zPPzd3wuDEejWsJr0h\nILDO7S1Hvfr+BWId4AcUVeXPwABAhTq4YvryiIHPtIq+dthx7kSqPjI2POD61xoDgFsEOwAA\nAI3g4gkAAACNINgBAABoBMEOAABAIwh2AAAAGkGwAwAA0AiCHQAAgEYQ7AAAADSCYAcAAKAR\nBDsAAACNINgBAABoBMEOAABAIwh2AAAAGkGwAwAA0AiCHQAAgEYQ7AAAADSCYAcAAKARBDsA\nAACNINgBAABoBMEOAABAIwh2AAAAGkGwAwAA0AiCHQAAgEYQ7AAAADSCYAcAAKARBDsAAACN\nINgBAABoBMEOAABAI/4fY5B79PfhnYUAAAAASUVORK5CYII="
     },
     "metadata": {
      "image/png": {
       "height": 420,
       "width": 420
      }
     },
     "output_type": "display_data"
    }
   ],
   "source": [
    "qqnorm(resid(M1))\n",
    "qqline(resid(M1))  \n",
    "shapiro.test(resid(M1))"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "0f96fac3",
   "metadata": {
    "papermill": {
     "duration": 0.010714,
     "end_time": "2024-07-27T21:21:56.126392",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.115678",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "* Homogeneity of variance:\n",
    "\n",
    "In this assumption the variance of data group should be equal. This can be checked by Bartlett Test. So, our Null hypothesis is that the variances across the four groups are statistically equal.\n",
    "\n",
    "Bartlett test:\n",
    "Bartlett's K-squared = 2.2509, df = 3, p-value = 0.522\n",
    "\n",
    "Bartlett's K-squared = 2.2509, This is the test statistic value, which reflects the sum of differences between the logarithms of observed variances and the logarithm of the pooled variance, weighted by their respective degrees of freedom.\n",
    "\n",
    "Degrees of Freedom (df) = 3, This represents the number of groups in our data minus one. Thus, its 3 since there are groups in test.\n",
    "\n",
    "P-value = 0.522: The observed value is above the significant value of 0.05, this implies that there is no statistical evidence to reject the null hypothesis. \n",
    "\n",
    "Therefore, in the case of Bartlett’s test all group variances are equal accepting the null hypothesis."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "id": "d401089e",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:56.152625Z",
     "iopub.status.busy": "2024-07-27T21:21:56.150890Z",
     "iopub.status.idle": "2024-07-27T21:21:56.177705Z",
     "shell.execute_reply": "2024-07-27T21:21:56.174675Z"
    },
    "papermill": {
     "duration": 0.044232,
     "end_time": "2024-07-27T21:21:56.181337",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.137105",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "\n",
      "\tBartlett test of homogeneity of variances\n",
      "\n",
      "data:  G3 by group\n",
      "Bartlett's K-squared = 2.2509, df = 3, p-value = 0.522\n",
      "\n"
     ]
    }
   ],
   "source": [
    "E1$group <- interaction(E1$sex, E1$higher)\n",
    "E1$group <- as.factor(E1$group)\n",
    "bartlett_result <- bartlett.test(G3 ~ group, data = E1)\n",
    "print(bartlett_result)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "199f316c",
   "metadata": {
    "papermill": {
     "duration": 0.011356,
     "end_time": "2024-07-27T21:21:56.203662",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.192306",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Aligned Rank Transform Test (Non - Parametric Test)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "id": "17e999b5",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:56.231132Z",
     "iopub.status.busy": "2024-07-27T21:21:56.229262Z",
     "iopub.status.idle": "2024-07-27T21:21:56.399971Z",
     "shell.execute_reply": "2024-07-27T21:21:56.397381Z"
    },
    "papermill": {
     "duration": 0.188103,
     "end_time": "2024-07-27T21:21:56.402804",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.214701",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Analysis of Variance of Aligned Rank Transformed Data\n",
      "\n",
      "Table Type: Anova Table (Type III tests) \n",
      "Model: No Repeated Measures (lm)\n",
      "Response: art(G3)\n",
      "\n",
      "             Df Df.res  F value     Pr(>F)    \n",
      "1 sex         1    645  8.23102  0.0042528  **\n",
      "2 higher      1    645 88.17204 < 2.22e-16 ***\n",
      "3 sex:higher  1    645  0.28566  0.5932002    \n",
      "---\n",
      "Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 \n"
     ]
    }
   ],
   "source": [
    "M_ART <- art(G3 ~ sex * higher, data = E1)\n",
    "\n",
    "R_Anova <- anova(M_ART)\n",
    "print(R_Anova)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "0f4d72e2",
   "metadata": {
    "papermill": {
     "duration": 0.011588,
     "end_time": "2024-07-27T21:21:56.425426",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.413838",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "A statistically significant disparity in grades (G3) exists between males and females. \n",
    "We reject the null hypothesis, which states that there is no difference in marks depending on sex, because the p-value is less than 0.05 and accept the alternative hypothesis.\n",
    "\n",
    "Willingness to pursue higher education (higher) has a hugely important impact on grades. Given the very low p-value, there is compelling evidence to reject the null hypothesis. This implies that the scores for individuals who have an education (\"yes\") and those who don't (\"no\") differ considerably. Thus, accepting the alternative hypothesis.\n",
    "\n",
    "There is no statistically significant relationship between sex and higher. This suggests that there is little difference in the impact of schooling on grades between males and females. Thus, accepting the null hypothesis that there is no interaction between the two variables sex and higher in relation to the dependent variable G3"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "b070ca77",
   "metadata": {
    "papermill": {
     "duration": 0.011049,
     "end_time": "2024-07-27T21:21:56.447664",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.436615",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Post-hoc Analysis\n",
    "\n",
    "We will concentrate on the pairwise comparisons within each significant main effect because the interaction effect was not significant. In this instance, Mann-Whitney U tests with multiple comparison adjustments can be used for the post-hoc analysis."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "id": "deef7925",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:56.474201Z",
     "iopub.status.busy": "2024-07-27T21:21:56.472385Z",
     "iopub.status.idle": "2024-07-27T21:21:56.506847Z",
     "shell.execute_reply": "2024-07-27T21:21:56.504599Z"
    },
    "papermill": {
     "duration": 0.05056,
     "end_time": "2024-07-27T21:21:56.509666",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.459106",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "\n",
      "\tWilcoxon rank sum test with continuity correction\n",
      "\n",
      "data:  m_group_sex and f_group_sex\n",
      "W = 42962, p-value = 0.0006317\n",
      "alternative hypothesis: true location shift is not equal to 0\n",
      "\n"
     ]
    }
   ],
   "source": [
    "yes_group_higher = E1$G3[E1$higher == \"yes\"]\n",
    "no_group_higher = E1$G3[E1$higher == \"no\"]\n",
    "\n",
    "m_group_sex = E1$G3[E1$sex == \"M\"]\n",
    "f_group_sex = E1$G3[E1$sex == \"F\"]\n",
    "\n",
    "\n",
    "mw_test_higher = wilcox.test(yes_group_higher, no_group_higher)\n",
    "mw_test_sex = wilcox.test(m_group_sex, f_group_sex)\n",
    "\n",
    "print(mw_test_sex)\n"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "ef648b64",
   "metadata": {
    "papermill": {
     "duration": 0.011197,
     "end_time": "2024-07-27T21:21:56.532231",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.521034",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "The main effect of sex was evident, we will compare the grades(G3) between females and males.\n",
    "\n",
    "Result: P-value is 0.0006317, W = 42962\n",
    "\n",
    "This implies that there is statistically significant difference in grades between females and males. Since P-value<0.05, We can say with confidence that average grades differ between sex/genders.\n",
    "\n",
    "W is value of test statistic; this is the sum of ranks for the one of the groups after ranking. Higher the value implies a shift in central tendency i.e one group have higher rank than other."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 10,
   "id": "11956e31",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:56.558578Z",
     "iopub.status.busy": "2024-07-27T21:21:56.556834Z",
     "iopub.status.idle": "2024-07-27T21:21:56.573563Z",
     "shell.execute_reply": "2024-07-27T21:21:56.571382Z"
    },
    "papermill": {
     "duration": 0.034069,
     "end_time": "2024-07-27T21:21:56.577704",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.543635",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "\n",
      "\tWilcoxon rank sum test with continuity correction\n",
      "\n",
      "data:  yes_group_higher and no_group_higher\n",
      "W = 33164, p-value < 2.2e-16\n",
      "alternative hypothesis: true location shift is not equal to 0\n",
      "\n"
     ]
    }
   ],
   "source": [
    "print(mw_test_higher)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "b3326a7e",
   "metadata": {
    "papermill": {
     "duration": 0.011169,
     "end_time": "2024-07-27T21:21:56.600490",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.589321",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "The main effect of ‘Higer’ was significant, we will compare the grades(G3) between females and males.\n",
    "\n",
    "Result: P-value is < 2.2e-16, W = 33164\n",
    "\n",
    "This implies that there is statistically strong significant difference in grades between those students who want to pursue higher education(yes) and those you don’t want to purse higher education. Given the magnitude of the difference the statistical significance, the influence of aspiration of higher education has substantial role on grades."
   ]
  },
  {
   "cell_type": "markdown",
   "id": "fddb51c1",
   "metadata": {
    "papermill": {
     "duration": 0.012344,
     "end_time": "2024-07-27T21:21:56.624339",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.611995",
     "status": "completed"
    },
    "tags": []
   },
   "source": []
  },
  {
   "cell_type": "markdown",
   "id": "1f531328",
   "metadata": {
    "papermill": {
     "duration": 0.011354,
     "end_time": "2024-07-27T21:21:56.647022",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.635668",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Permutation Test\n",
    "\n",
    "A permutation test is a non-parametric method to find out the observed difference between groups are by chance.\n",
    "\n",
    "We will perform permutation test on the difference we observed on grades based on willingness to purse higher education. The null hypothesis is there is no real difference between the groups.\n",
    "\n",
    "Result: Original difference in Means is 3.478761 and P-value of permutation test is 0.\n",
    "\n",
    "This indicates there is substantial difference in between the groups a of higher. This implies the mean grades for students with higher “yes” and “no” differ from significant margin.\n",
    "\n",
    "A P-value of 0 suggest that permuted dataset did not produce a test statistic as extreme as the one calculated in actual data. This is a rather strong indication that the effect is very unlikely to occur under null hypothesis. Therefore, null hypothesis is rejected."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 11,
   "id": "b4fe4941",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2024-07-27T21:21:56.674199Z",
     "iopub.status.busy": "2024-07-27T21:21:56.672498Z",
     "iopub.status.idle": "2024-07-27T21:21:58.948995Z",
     "shell.execute_reply": "2024-07-27T21:21:58.946491Z"
    },
    "papermill": {
     "duration": 2.29393,
     "end_time": "2024-07-27T21:21:58.952173",
     "exception": false,
     "start_time": "2024-07-27T21:21:56.658243",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<strong>yes:</strong> 3.47876061969015"
      ],
      "text/latex": [
       "\\textbf{yes:} 3.47876061969015"
      ],
      "text/markdown": [
       "**yes:** 3.47876061969015"
      ],
      "text/plain": [
       "     yes \n",
       "3.478761 "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    },
    {
     "data": {
      "text/html": [
       "0"
      ],
      "text/latex": [
       "0"
      ],
      "text/markdown": [
       "0"
      ],
      "text/plain": [
       "[1] 0"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "set.seed(123)  \n",
    "\n",
    "og_difference <- abs(diff(tapply(E1$G3, E1$higher, mean)))\n",
    "\n",
    "n_permutations <- 10000\n",
    "p_difference <- replicate(n_permutations, {\n",
    "  shuffled_higher <- sample(E1$higher)\n",
    "  p_difference <- abs(diff(tapply(E1$G3, shuffled_higher, mean)))\n",
    "  p_difference\n",
    "})\n",
    "\n",
    "\n",
    "p_value <- mean(p_difference  >= og_difference)\n",
    "\n",
    "\n",
    "og_difference\n",
    "p_value"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "b31b6f94",
   "metadata": {
    "papermill": {
     "duration": 0.016552,
     "end_time": "2024-07-27T21:21:58.985119",
     "exception": false,
     "start_time": "2024-07-27T21:21:58.968567",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Results\n",
    "Comparing Mann-Whitney U for Higher education and Permutation test:\n",
    "\n",
    "The significant point we can conclude that both 'sex' and 'higher' independently affect 'G3' i.e. final grade. However, the non-significant interaction within the group means the impact of education on marks is similar for both males and females; in other words, aspiration of higher education influences grades regardless of sex.\n",
    "\n",
    "Statistical Significance: There is sufficient proof from both tests that willingness to pursue higher education affects final grades of secondary students. The significantly low p-values in both tests showcase the robustness of through different statistical approaches.\n",
    "\n",
    "There is no strong statistical evidence that together both sex and willingness to purse higher education has an effect on final grades."
   ]
  },
  {
   "cell_type": "markdown",
   "id": "56baadbc",
   "metadata": {
    "papermill": {
     "duration": 0.011705,
     "end_time": "2024-07-27T21:21:59.010392",
     "exception": false,
     "start_time": "2024-07-27T21:21:58.998687",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "# Conclusion \n",
    "\n",
    "The notion that aspiration of pursing higher education is a significant factor in influencing academic performance as determined by marks is supported by the uniformity of these several statistical tests.\n",
    "\n",
    "These results suggest that interventions or policies aimed at promoting the benefits of higher education and improving higher education accessibility for students from all backgrounds irrespective of their genders. \n",
    "\n",
    "The significance of sex as a factor also suggests that there may be different strategies or considerations needed to address gender disparities in educational outcomes.\n"
   ]
  }
 ],
 "metadata": {
  "kaggle": {
   "accelerator": "none",
   "dataSources": [
    {
     "datasetId": 4227127,
     "sourceId": 7290869,
     "sourceType": "datasetVersion"
    }
   ],
   "dockerImageVersionId": 30749,
   "isGpuEnabled": false,
   "isInternetEnabled": true,
   "language": "r",
   "sourceType": "notebook"
  },
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "4.4.0"
  },
  "papermill": {
   "default_parameters": {},
   "duration": 11.815981,
   "end_time": "2024-07-27T21:21:59.145298",
   "environment_variables": {},
   "exception": null,
   "input_path": "__notebook__.ipynb",
   "output_path": "__notebook__.ipynb",
   "parameters": {},
   "start_time": "2024-07-27T21:21:47.329317",
   "version": "2.6.0"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
