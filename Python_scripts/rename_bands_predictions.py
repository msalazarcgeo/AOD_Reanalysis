import just_todo_process_py
import argparse
## python rename_bands_predictions.py --prediction_path "../datos/raster/ZMVM_predictions_9PBLH/2017_bis/"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Rename the bands of all the file in directory')
    parser.add_argument('--prediction_path',
                         help= "Path to the predictions folder")
    args = parser.parse_args()


    just_todo_process_py.set_bands_dir(args.prediction_path)
