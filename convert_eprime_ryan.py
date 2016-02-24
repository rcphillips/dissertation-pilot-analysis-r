import os
#import pickle
import inspect
import csv
import numpy.core.fromnumeric as fn
import sys

def text_to_csv(text_file, out_file):
    """
    Converts text file produced by successful completion of E-Prime experiment
    to csv. Output from text_to_csv can be used to deduce information necessary
    for text_to_rcsv (e.g. columns to merge, columns to rename, etc.).

    Inputs:
    text_file:  Text file created automatically by successful completion of E-
                Prime experiment (along with the edat file). If the experiment
                was stopped early or otherwise did not complete 100% successfully,
                this file may need to be edited before being run through this
                function.
    out_file:   Output csv file. Should have csv suffix.

    Outputs:
    No returned variables, but does create out_file.
    """

    # Load the text file as a list.
    with open(text_file, "r") as fo:
        text_data = list(fo)

    # Remove unicode characters.
    filtered_data = [_strip(row) for row in text_data]

    # Determine where rows begin and end.
    start_index = [i_row for i_row, row in enumerate(filtered_data)
                   if row == "*** LogFrame Start ***"]
    end_index = [i_row for i_row, row in enumerate(filtered_data)
                 if row == "*** LogFrame End ***"]
    if (len(start_index) != len(end_index) or start_index[0] >= end_index[0]):
        print("Warning: LogFrame Starts and Ends do not match up.")
    n_rows = min(len(start_index), len(end_index))

    # Find column headers and remove duplicates.
    all_headers = []
    data_by_rows = []

    for i_row in range(n_rows):
        one_row = filtered_data[start_index[i_row]+1:end_index[i_row]]
        data_by_rows.append(one_row)
        for j_col in range(len(one_row)):
            split_header_idx = one_row[j_col].index(": ")
            all_headers.append(one_row[j_col][:split_header_idx])

    unique_headers = list(set(all_headers))

    # Preallocate list of lists composed of NULLs.
    null_col = ["NULL"] * (n_rows+1)
    data_matrix = [null_col[:] for i_col in range(len(unique_headers))]

    # Fill list of lists with relevant data from data_by_rows and
    # unique_headers.
    for i_col in range(len(unique_headers)):
        data_matrix[i_col][0] = unique_headers[i_col]

    for i_row in range(n_rows):
        for j_col in range(len(data_by_rows[i_row])):
            split_header_idx = data_by_rows[i_row][j_col].index(": ")
            for k_header in range(len(unique_headers)):
                if (data_by_rows[i_row][j_col][:split_header_idx] ==
                        unique_headers[k_header]):
                    data_matrix[k_header][i_row + 1] = (data_by_rows[i_row]
                                                        [j_col]
                                                        [split_header_idx+2:])

    # If a column is all NULLs except for the header and one value at the
    # bottom, fill the column up with that bottom value.
    for i_col, col in enumerate(data_matrix):
        rows_w_vals = [j_cell for j_cell, cell in enumerate(col) if
                       cell != "NULL"]
        # If the column is full of NULLs (except for the last row and the header), len(row_w_vals) = 2
        if len(rows_w_vals) == 2 and (rows_w_vals[1] == len(col) - 1):
            data_matrix[i_col][1:len(col)] = ([col[rows_w_vals[1]]] * (len(col) - 1))

        data_matrix[i_col] = col[:len(col) - 2]

    # Transpose data_matrix.
    out_matrix = _transpose(data_matrix)

    try:
        fo = open(out_file, 'wb')
        file_ = csv.writer(fo)
        for row in out_matrix:
            file_.writerow(row)

        print("Output file successfully created- {0}".format(out_file))
    except IOError:
        print("Can't open output file- {0}".format(out_file))
    finally:
        fo.close()

    print("Saved {0}".format(out_file))



def _det_file_type(in_file):
    """
    Determines number of lines to remove and file delimiter from filetype.
    """
    [fn, sf] = os.path.splitext(in_file)
    if sf == ".csv":
        delimiter_ = ","
        rem_lines = 0
    elif sf == ".txt":
        delimiter_ = "\t"
        rem_lines = 3
    elif len(in_file) == 0:
        raise ValueError("Input file name is empty.")
    else:
        raise ValueError("Input file name is not .csv or .txt.")

    return delimiter_, rem_lines


def _merge_lists(lists, option):
    """
    Merges multiple lists into one list, with the default being the values of
    the first list. It either replaces values with NULL if NULL is in that
    position in another list or replaces NULL with values if values are in that
    position in another list.
    """
    if type(lists[0]) != list:
        return lists
    else:
        merged = lists[0]
        for i_col in range(len(lists)):
            if option == "all_null":
                merged = [lists[i_col][i_row] if lists[i_col][i_row] == "NULL"
                          else merged[i_row] for i_row in range(len(merged))]
            elif option == "all_else":
                merged = [lists[i_col][i_row] if lists[i_col][i_row] != "NULL"
                          else merged[i_row] for i_row in range(len(merged))]
        return merged


def _strip(string):
    """
    Removes unicode characters in string.
    """
    return "".join([val for val in string if 31 < ord(val) < 127])


def _transpose(list_):
    """
    Transposes a list of lists.
    """
    transposed_ = [[row[col] for row in list_] for col in range(len(list_[0]))]
    transposed = [col for col in transposed_ if col]
    return transposed


def _try_index(list_, val):
    """
    Indexes a list without throwing an error if the value isn't found.
    """
    try:
        return list_.index(val)
    except:
        # print(val)
        pass




text_to_csv("150609_CART_send5forscanner_withtracking-44-1.txt","subj44_task_test.csv")