
import os
import sys
import pandas as pd
import time
from tqdm import tqdm
import concurrent.futures
import traceback
import logging
import gc

# Configure logging with UTF-8 encoding for both file and stream handlers
def configure_logging():
    stream_handler = logging.StreamHandler(sys.stdout)
    stream_handler.setFormatter(logging.Formatter("%(asctime)s - %(levelname)s: %(message)s"))
    file_handler = logging.FileHandler('excel_merger_log.txt', encoding='utf-8')
    file_handler.setFormatter(logging.Formatter("%(asctime)s - %(levelname)s: %(message)s"))
    logging.getLogger().handlers = [file_handler, stream_handler]
    logging.getLogger().setLevel(logging.INFO)

configure_logging()

# Global configuration
main_folder = r"C:\Users\abels\Documents\MyR\RSCA_NoDB\output\SoCal_Mod"
summary_sheet_names = [
    "Module Summary",
    "LOA Summary",
    "Reference Condition Comparison",
    "Stressor Response Summary",
    "Spatial Co-Occurrence Summary",
    "RSCA Comparator Site Data"
]

# Function to process a single file with detailed error handling
def process_file(file_path):
    result = {
        'monitoring': None,
        'summary_sheets': {sheet: None for sheet in summary_sheet_names},
        'error': None
    }
    try:
        file = os.path.basename(file_path)
        
        # Process "Monitoring_Recommendations" files
        if "Monitoring_Recommendations" in file:
            try:
                df = pd.read_excel(file_path, engine="openpyxl")
                df['Source_File'] = file
                result['monitoring'] = df
            except Exception as e:
                result['error'] = f"Monitoring file error: {str(e)}"
                logging.error(f"Error processing {file_path}:\n{traceback.format_exc()}")
        
        # Process "Summary_Site_Data" files
        elif "Summary_Site_Data" in file:
            try:
                try:
                    # Try reading specific sheets first
                    sheets = pd.read_excel(file_path, sheet_name=summary_sheet_names, engine="openpyxl")
                    for sheet in summary_sheet_names:
                        if sheet in sheets:
                            result['summary_sheets'][sheet] = sheets[sheet]
                except ValueError:
                    # If specific sheets fail, read all sheets and pick the ones we need
                    sheets = pd.read_excel(file_path, sheet_name=None, engine="openpyxl")
                    for sheet in summary_sheet_names:
                        if sheet in sheets:
                            result['summary_sheets'][sheet] = sheets[sheet]
            except Exception as e:
                result['error'] = f"Summary file error: {str(e)}"
                logging.error(f"Error processing {file_path}:\n{traceback.format_exc()}")
    except Exception as e:
        result['error'] = f"Unexpected error: {str(e)}"
        logging.error(f"Unexpected error processing {file_path}:\n{traceback.format_exc()}")
    return result

if __name__ == '__main__':
    start_time = time.time()

    # Find all Excel files in the specified directory (and subdirectories)
    excel_files = []
    for root, _, files in os.walk(main_folder):
        for file in files:
            if file.endswith('.xlsx'):
                excel_files.append(os.path.join(root, file))
    logging.info(f"Found {len(excel_files)} Excel files to process")

    # Prepare lists to collect results
    monitoring_list = []
    merged_summary_data = {sheet: [] for sheet in summary_sheet_names}
    error_files = []

    # Process files in batches using parallel processing
    batch_size = 100
    num_batches = (len(excel_files) + batch_size - 1) // batch_size

    for batch_num in range(num_batches):
        start_idx = batch_num * batch_size
        batch = excel_files[start_idx:start_idx + batch_size]
        logging.info(f"Processing batch {batch_num + 1}/{num_batches}")
        
        # Create a new process pool for this batch
        with concurrent.futures.ProcessPoolExecutor(max_workers=max(1, os.cpu_count() - 1)) as executor:
            try:
                results = list(tqdm(executor.map(process_file, batch), total=len(batch),
                                    desc=f"Batch {batch_num + 1}"))
                # Collect results using enumerate to map each result with its file
                for i, result in enumerate(results):
                    file_processed = batch[i]
                    if result['error']:
                        error_files.append({'file': file_processed, 'error': result['error']})
                    if result['monitoring'] is not None:
                        monitoring_list.append(result['monitoring'])
                    for sheet in summary_sheet_names:
                        if result['summary_sheets'][sheet] is not None:
                            merged_summary_data[sheet].append(result['summary_sheets'][sheet])
            except Exception as e:
                logging.error(f"Batch {batch_num + 1} processing error:\n{traceback.format_exc()}")
        gc.collect()

    logging.info(f"Finished processing all files in {time.time() - start_time:.1f} seconds")

    # Log errors to a file if any were encountered
    if error_files:
        logging.warning(f"Encountered errors in {len(error_files)} files")
        with open(os.path.join(main_folder, 'error_log.txt'), 'w', encoding='utf-8') as f:
            for error_file in error_files:
                f.write(f"File: {error_file['file']}\nError: {error_file['error']}\n\n")

    # Save merged Monitoring_Recommendations data if available
    if monitoring_list:
        logging.info("Merging monitoring data...")
        merged_monitoring = pd.concat(monitoring_list, ignore_index=True)
        output_monitoring = os.path.join(main_folder, "Merged_Monitoring_Recommendations.csv")
        merged_monitoring.to_csv(output_monitoring, index=False)
        logging.info(f"✅ Merged Monitoring CSV saved to: {output_monitoring}")
    else:
        logging.warning("❌ No 'Monitoring_Recommendations' files found.")

    # Save each Summary_Site_Data sheet separately
    for sheet, df_list in merged_summary_data.items():
        if df_list:
            logging.info(f"Merging '{sheet}' data...")
            merged_df = pd.concat(df_list, ignore_index=True)
            output_csv = os.path.join(main_folder, f"Merged_{sheet.replace(' ', '_')}.csv")
            merged_df.to_csv(output_csv, index=False)
            logging.info(f"✅ Merged CSV saved for '{sheet}': {output_csv}")
        else:
            logging.warning(f"❌ No data found for '{sheet}'")

    logging.info(f"Total execution time: {time.time() - start_time:.1f} seconds")
=======
import os
import sys
import pandas as pd
import time
from tqdm import tqdm
import concurrent.futures
import traceback
import logging
import gc

# Configure logging with UTF-8 encoding for both file and stream handlers
def configure_logging():
    stream_handler = logging.StreamHandler(sys.stdout)
    stream_handler.setFormatter(logging.Formatter("%(asctime)s - %(levelname)s: %(message)s"))
    file_handler = logging.FileHandler('excel_merger_log.txt', encoding='utf-8')
    file_handler.setFormatter(logging.Formatter("%(asctime)s - %(levelname)s: %(message)s"))
    logging.getLogger().handlers = [file_handler, stream_handler]
    logging.getLogger().setLevel(logging.INFO)

configure_logging()

# Global configuration
main_folder = r"C:\Users\abels\Documents\MyR\RSCA_NoDB\output\Cal_Mod"
summary_sheet_names = [
    "Module Summary",
    "LOA Summary",
    "Reference Condition Comparison",
    "Stressor Response Summary",
    "Spatial Co-Occurrence Summary",
    "RSCA Comparator Site Data"
]

# Function to process a single file with detailed error handling
def process_file(file_path):
    result = {
        'monitoring': None,
        'summary_sheets': {sheet: None for sheet in summary_sheet_names},
        'error': None
    }
    try:
        file = os.path.basename(file_path)
        
        # Process "Monitoring_Recommendations" files
        if "Monitoring_Recommendations" in file:
            try:
                df = pd.read_excel(file_path, engine="openpyxl")
                df['Source_File'] = file
                result['monitoring'] = df
            except Exception as e:
                result['error'] = f"Monitoring file error: {str(e)}"
                logging.error(f"Error processing {file_path}:\n{traceback.format_exc()}")
        
        # Process "Summary_Site_Data" files
        elif "Summary_Site_Data" in file:
            try:
                try:
                    # Try reading specific sheets first
                    sheets = pd.read_excel(file_path, sheet_name=summary_sheet_names, engine="openpyxl")
                    for sheet in summary_sheet_names:
                        if sheet in sheets:
                            result['summary_sheets'][sheet] = sheets[sheet]
                except ValueError:
                    # If specific sheets fail, read all sheets and pick the ones we need
                    sheets = pd.read_excel(file_path, sheet_name=None, engine="openpyxl")
                    for sheet in summary_sheet_names:
                        if sheet in sheets:
                            result['summary_sheets'][sheet] = sheets[sheet]
            except Exception as e:
                result['error'] = f"Summary file error: {str(e)}"
                logging.error(f"Error processing {file_path}:\n{traceback.format_exc()}")
    except Exception as e:
        result['error'] = f"Unexpected error: {str(e)}"
        logging.error(f"Unexpected error processing {file_path}:\n{traceback.format_exc()}")
    return result

if __name__ == '__main__':
    start_time = time.time()

    # Find all Excel files in the specified directory (and subdirectories)
    excel_files = []
    for root, _, files in os.walk(main_folder):
        for file in files:
            if file.endswith('.xlsx'):
                excel_files.append(os.path.join(root, file))
    logging.info(f"Found {len(excel_files)} Excel files to process")

    # Prepare lists to collect results
    monitoring_list = []
    merged_summary_data = {sheet: [] for sheet in summary_sheet_names}
    error_files = []

    # Process files in batches using parallel processing
    batch_size = 100
    num_batches = (len(excel_files) + batch_size - 1) // batch_size

    for batch_num in range(num_batches):
        start_idx = batch_num * batch_size
        batch = excel_files[start_idx:start_idx + batch_size]
        logging.info(f"Processing batch {batch_num + 1}/{num_batches}")
        
        # Create a new process pool for this batch
        with concurrent.futures.ProcessPoolExecutor(max_workers=max(1, os.cpu_count() - 1)) as executor:
            try:
                results = list(tqdm(executor.map(process_file, batch), total=len(batch),
                                    desc=f"Batch {batch_num + 1}"))
                # Collect results using enumerate to map each result with its file
                for i, result in enumerate(results):
                    file_processed = batch[i]
                    if result['error']:
                        error_files.append({'file': file_processed, 'error': result['error']})
                    if result['monitoring'] is not None:
                        monitoring_list.append(result['monitoring'])
                    for sheet in summary_sheet_names:
                        if result['summary_sheets'][sheet] is not None:
                            merged_summary_data[sheet].append(result['summary_sheets'][sheet])
            except Exception as e:
                logging.error(f"Batch {batch_num + 1} processing error:\n{traceback.format_exc()}")
        gc.collect()

    logging.info(f"Finished processing all files in {time.time() - start_time:.1f} seconds")

    # Log errors to a file if any were encountered
    if error_files:
        logging.warning(f"Encountered errors in {len(error_files)} files")
        with open(os.path.join(main_folder, 'error_log.txt'), 'w', encoding='utf-8') as f:
            for error_file in error_files:
                f.write(f"File: {error_file['file']}\nError: {error_file['error']}\n\n")

    # Save merged Monitoring_Recommendations data if available
    if monitoring_list:
        logging.info("Merging monitoring data...")
        merged_monitoring = pd.concat(monitoring_list, ignore_index=True)
        output_monitoring = os.path.join(main_folder, "Merged_Monitoring_Recommendations.csv")
        merged_monitoring.to_csv(output_monitoring, index=False)
        logging.info(f"✅ Merged Monitoring CSV saved to: {output_monitoring}")
    else:
        logging.warning("❌ No 'Monitoring_Recommendations' files found.")

    # Save each Summary_Site_Data sheet separately
    for sheet, df_list in merged_summary_data.items():
        if df_list:
            logging.info(f"Merging '{sheet}' data...")
            merged_df = pd.concat(df_list, ignore_index=True)
            output_csv = os.path.join(main_folder, f"Merged_{sheet.replace(' ', '_')}.csv")
            merged_df.to_csv(output_csv, index=False)
            logging.info(f"✅ Merged CSV saved for '{sheet}': {output_csv}")
        else:
            logging.warning(f"❌ No data found for '{sheet}'")

    logging.info(f"Total execution time: {time.time() - start_time:.1f} seconds")
