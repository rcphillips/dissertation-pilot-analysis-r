# -*- coding: utf-8 -*-
"""
Created on Tue Jul 21 10:14:18 2015

@author: rcphillips
"""
import numpy
from pandas import DataFrame, Series

countries = ['Russian Fed.', 'Norway', 'Canada', 'United States',
             'Netherlands', 'Germany', 'Switzerland', 'Belarus',
             'Austria', 'France', 'Poland', 'China', 'Korea', 
             'Sweden', 'Czech Republic', 'Slovenia', 'Japan',
             'Finland', 'Great Britain', 'Ukraine', 'Slovakia',
             'Italy', 'Latvia', 'Australia', 'Croatia', 'Kazakhstan']

gold = [13, 11, 10, 9, 8, 8, 6, 5, 4, 4, 4, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
silver = [11, 5, 10, 7, 7, 6, 3, 0, 8, 4, 1, 4, 3, 7, 4, 2, 4, 3, 1, 0, 0, 2, 2, 2, 1, 0]
bronze = [9, 10, 5, 12, 9, 5, 2, 1, 5, 7, 1, 2, 2, 6, 2, 4, 3, 1, 2, 1, 0, 6, 2, 1, 0, 1]

olympic_medal_counts = {'country_name':Series(countries),
                        'gold': Series(gold),
                        'silver': Series(silver),
                        'bronze': Series(bronze)}
df = DataFrame(olympic_medal_counts)
values = [4,2,1]
medals = df[['gold','silver','bronze']]
points = numpy.dot(medals,values)
print points


olympic_points_df = {'country_name':Series(countries),
                     'points':Series(points)}

print avg_medal_count

#
#
#def avg_medal_count():
#    '''
#    '''    
#
#    return avg_medal_count
#
#
