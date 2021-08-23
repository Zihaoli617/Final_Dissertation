import numpy as np
import scipy
from scipy.stats import norm
import pandas as pd
import arviz as az
import pymc3 as pm


def correlations():

    # Load nutrition data
    df_tesco_msoa = pd.read_csv('year_msoa_grocery.csv', encoding='utf-8', header=0)
    df_tesco_ward = pd.read_csv('year_osward_grocery.csv', encoding='utf-8', header=0)
    df_tesco_oslaua = pd.read_csv('year_borough_grocery.csv', encoding='utf-8', header=0)

    # Load health data
    df_child_obesity_ward = pd.read_csv('child_obesity_london_ward_2013-2014.csv', encoding='utf-8', header=0).dropna()
    df_child_obesity_oslaua = pd.read_csv('child_obesity_london_oslaua_2015-2016.csv', encoding='utf-8', header=0).dropna()
    df_adult_obesity_oslaua = pd.read_csv('london_obesity_oslaua_2012.csv', encoding='utf-8', header=0).dropna()
    df_adult_obesity_hospital_oslaua = pd.read_csv('obesity_hospitalization_oslaua_2016.csv', encoding='utf-8',
                                                   header=0).dropna()
    df_diabetes_ward = pd.read_csv('diabetes_estimates_osward_2016.csv', encoding='utf-8', header=0).dropna()

    outfile = open('correlations_health_outcomes.csv', 'wt', encoding='utf-8')
    outfile.write('outcome,nutrient,r,p\n')

    food_indicators = ['energy_tot', 'energy_fat', 'energy_saturate', 'energy_sugar', 'energy_protein', 'energy_carb',
                       'energy_fibre', 'h_nutrients_calories_norm']
    food_indicators_labels = ['energy', 'fat', 'saturate', 'sugar', 'protein', 'carb', 'fibre', 'diversity']

    # Correlation with child obesity at Ward level
    df_join = df_tesco_ward.merge(df_child_obesity_ward, how='inner')
    plot_labels = ['Prevalence of overweight\nchildren (reception)', 'Prevalence of overweight\nchildren (year 6)',
                   'Prevalence of obese\nchildren (reception)', 'Prevalence of obese\nchildren (year 6)']
    outcomes = ['f_overweight_5y', 'f_overweight_11y', 'f_obese_5y', 'f_obese_11y']

    for b, lab in zip(outcomes, plot_labels):
        correl = []
        nutrient = []
        for a, al in zip(food_indicators, food_indicators_labels):
            r, p = scipy.stats.spearmanr(df_join[a], df_join[b])
            if p < 0.05:
                outfile.write('%s,%s,%s,%s\n' % (b, al, r, p))
                correl.append(r)
                nutrient.append(al)

    # Correlation with adult obesity
    df_join = df_tesco_oslaua.merge(df_adult_obesity_oslaua, how='inner')
    plot_labels = ['Prevalence of\noverweight adults', 'Prevalence of\nobese adults']
    outcomes = ['f_overweight', 'f_obese']
    for b, lab in zip(outcomes, plot_labels):
        correl = []
        nutrient = []
        for a, al in zip(food_indicators, food_indicators_labels):
            r, p = scipy.stats.spearmanr(df_join[a], df_join[b])
            if p < 0.05:
                # print(b,al,r,p)
                outfile.write('%s,%s,%s,%s\n' % (b, al, r, p))
                correl.append(r)
                nutrient.append(al)

    # Correlation with diabetes estimates
    df_join_diabetes = df_tesco_ward.merge(df_diabetes_ward, how='inner')
    plot_labels = ['Diabetes\nprevalence']
    outcomes = ['estimated_diabetes_prevalence']
    for b, lab in zip(outcomes, plot_labels):
        correl = []
        nutrient = []
        for a, al in zip(food_indicators, food_indicators_labels):
            r, p = scipy.stats.spearmanr(df_join_diabetes[a], df_join_diabetes[b])
            if p < 0.05:
                # print(b,al,r,p)
                outfile.write('%s,%s,%s,%s\n' % (b, al, r, p))
                correl.append(r)
                nutrient.append(al)

    outfile.close()


def bayesian_regression():
    df_grocery = pd.read_csv('year_ward_grocery.csv')
    df_grocery['female_perc'] = df_grocery.apply(lambda row: row['female'] / row['population'], axis=1)
    df_diabetes = pd.read_csv('diabetes_estimates_osward_2016.csv', encoding='utf-8', header=0).dropna()
    df_geo = pd.read_csv('london_pcd2geo_2015.csv', encoding='utf-8')
    df_geo = df_geo[['osward','oslaua']]
    df_geo = df_geo.drop_duplicates()

    df = df_grocery.merge(df_diabetes, how='inner', left_on='area_id', right_on='osward')
    df = df.merge(df_geo, how='inner', on='osward')

    plt.figure(figsize=(8, 8))
    plt.plot(df['energy_carb'], df['estimated_diabetes_prevalence'], 'bo')
    plt.xlabel(f'energy_carb', size = 18)
    plt.ylabel(f'estimated_diabetes_prevalence', size = 18)

    X1=df['energy_carb'].values
    X2=df['h_energy_nutrients_norm'].values
    X3=df['avg_age'].values
    X4=df['female_perc'].values
    X5=df['num_transactions'].values
    X6=df['people_per_sq_km'].values

    X5 = np.array([np.log2(x) for x in X5])
    X6 = np.array([np.log2(x) for x in X6])

    Y=df['estimated_diabetes_prevalence'].values

    oslaua2index = {}
    i=0
    for v in df['oslaua'].values:
        if v not in oslaua2index:
            oslaua2index[v]=i
            i += 1

    df['oslaua_idx'] = df.apply(lambda row : oslaua2index[row['oslaua']], axis=1)
        
    n_oslauas = n_counties = len(df['oslaua_idx'].unique())
    oslaua_idx = df['oslaua_idx'].values

    hierarchical_model = pm.Model()
    with hierarchical_model:
        # Hyperpriors for group nodes
        mu_a = pm.Normal('mu_a', mu=0., sigma=100)
        sigma_a = pm.HalfNormal('sigma_a', 5.)
        a = pm.Normal('a', mu=mu_a, sigma=sigma_a, shape=n_oslauas)
        
        mu_b1 = pm.Normal('mu_b1', mu=0., sigma=100)
        sigma_b1 = pm.HalfNormal('sigma_b1', 5.)
        b1 = pm.Normal('b1', mu=mu_b1, sigma=sigma_b1, shape=n_oslauas)
        
        mu_b2 = pm.Normal('mu_b2', mu=0., sigma=100)
        sigma_b2 = pm.HalfNormal('sigma_b2', 5.)
        b2 = pm.Normal('b2', mu=mu_b2, sigma=sigma_b2, shape=n_oslauas)
        
        mu_b3 = pm.Normal('mu_b3', mu=0., sigma=100)
        sigma_b3 = pm.HalfNormal('sigma_b3', 5.)
        b3 = pm.Normal('b3', mu=mu_b3, sigma=sigma_b3, shape=n_oslauas)
        
        mu_b4 = pm.Normal('mu_b4', mu=0., sigma=100)
        sigma_b4 = pm.HalfNormal('sigma_b4', 5.)
        b4 = pm.Normal('b4', mu=mu_b4, sigma=sigma_b4, shape=n_oslauas)
        
        mu_b5 = pm.Normal('mu_b5', mu=0., sigma=100)
        sigma_b5 = pm.HalfNormal('sigma_b5', 5.)
        b5 = pm.Normal('b5', mu=mu_b5, sigma=sigma_b5, shape=n_oslauas)
        
        mu_b6 = pm.Normal('mu_b6', mu=0., sigma=100)
        sigma_b6 = pm.HalfNormal('sigma_b6', 5.)
        b6 = pm.Normal('b6', mu=mu_b6, sigma=sigma_b6, shape=n_oslauas)
        
        # Model error
        eps = pm.HalfCauchy('eps', 5.)
        
        estimate = a[oslaua_idx] + b1[oslaua_idx]*X1 + b2[oslaua_idx]*X2 + b3[oslaua_idx]*X3 + b4[oslaua_idx]*X4 + b5[oslaua_idx]*X5 + b6[oslaua_idx]*X6

        # Likelihood (sampling distribution) of observations
        likelihood = pm.Normal('likelihood', mu=estimate, sigma=eps, observed=Y)
        
        hierarchical_trace = pm.sample(10000, tune=10000, target_accept=.9)
        
    ppc = pm.sample_posterior_predictive(hierarchical_trace, samples=10000, model=hierarchical_model)
    np.asarray(ppc['likelihood']).shape
    print(az.r2_score(Y, ppc['likelihood']))


correlations()

bayesian_regression()