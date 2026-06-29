mport uproot
import matplotlib.pyplot as plt
import pandas as pd
import ipywidgets as widgets

class Events:
    def __init__(self, event_tree, base_var):
        self.event_tree = event_tree    # The raw event tree from the root file
        self.base_var = base_var        # The base variable to be plotted. This will be 'cut' by other variables
        self._variables = []            # List of strings; the names of variables which will be used
        self.df = []                    # Pandas dataframe to contain the data from selected variables
        self.cut_mask = []              # Pandas boolean mask used to create a list of cut entries

        self._variables.append(base_var)

    @property
    def variables(self):
        return self._variables
    
    @variables.setter
    def variables(self, vars=[]):
        '''Takes in a list of cutter variables and appends them to variables'''
        self.variables.extend(vars)

    def create_dataframe(self):
        '''Creates a pandas dataframe from the list of variables'''
        self.df = self.event_tree.arrays(self.variables, library="pd")

        self.cut_mask = pd.Series(True, index=self.df.index)

    def reset_mask(self):
        self.cut_mask = pd.Series(True, index=self.df.index)

    def filter_rows_range(self, var, min, max):
        '''Takes in a variable name string and a min/max and removes any rows where that variable's value does not fall within the range'''
        if var not in self._variables:
            raise Exception(f"Error: Cutter variable {var} is not in list of variables")

        self.reset_mask()
        self.cut_mask &= self.df[var].between(min, max, inclusive="both")

    def filter_rows_bool(self, var, boolean):
        '''Takes in a variable name string and a true/false and removes any rows where that variable's value does not match the boolean'''
        if var not in self._variables:
            raise Exception(f"Error: Cutter variable {var} is not in list of variables")
        elif not isinstance(boolean, bool):
            raise TypeError("Error: input must be a boolean")
        
        self.cut_mask &= self.df[var] == boolean
        
    def is_bool_variable(self, var):
        '''Checks whether or not a cutter variable contains boolean data. If not, it will always contain float data instead.'''
        if var not in self._variables:
            raise Exception(f"Error: Cutter variable {var} is not in list of variables")
        
        for x in self.df[var].head(100):    # Checks the variable's first 100 entries to see if any of them are non-boolean. 100 might be overkill but whatever
            if not (x == 1 or x == 0):
                return False
        
        return True


class PlotHelper:
    def __init__(self, events):
        self.events = events        # Takes in an Events object so PlotHelper can access its data

    def draw_plots(self):
        '''Placeholder function, all of this will be replaced'''
        arr_before = self.events.df[self.events.base_var]
        arr_after = self.events.df[self.events.cut_mask][self.events.base_var]

        plt.subplots(figsize=(15, 5))
        plt.subplot(2,1,1)
        plt_before = plt.hist(arr_before, bins=500)

        plt.subplot(2,1,2)
        plt_after = plt.hist(arr_after, bins=500)

        plt.show()
                