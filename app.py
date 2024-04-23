from vetiver import VetiverModel
from dotenv import load_dotenv, find_dotenv
import vetiver
import pins

load_dotenv(find_dotenv())

b = pins.board_folder('Final Project/data/model', allow_pickle_read=True)
v = VetiverModel.from_pin(b, 'penguin_model', version = '20240310T160931Z-a6f9b')

vetiver_api = vetiver.VetiverAPI(v)
api = vetiver_api.app

import requests

req_data = {
  "bill_length_mm": 0,
  "species_Chinstrap": False,
  "species_Gentoo": False,
  "sex_male": False
}
req = requests.post('http://127.0.0.1:8080/predict', json = [req_data])
res = req.json().get('predict')[0]

import logging

logging.basicConfig(
    format='%(asctime)s - %(message)s',
    level=logging.INFO
)
from shiny import App, render, ui, reactive
import requests

api_url = 'http://127.0.0.1:8080/predict'

app_ui = ui.page_fluid(
    ui.panel_title("Penguin Mass Predictor"), 
    ui.layout_sidebar(
        ui.panel_sidebar(
            [ui.input_slider("bill_length", "Bill Length (mm)", 30, 60, 45, step = 0.1),
            ui.input_select("sex", "Sex", ["Male", "Female"]),
            ui.input_select("species", "Species", ["Adelie", "Chinstrap", "Gentoo"]),
            ui.input_action_button("predict", "Predict")]
        ),
        ui.panel_main(
            ui.h2("Penguin Parameters"),
            ui.output_text_verbatim("vals_out"),
            ui.h2("Predicted Penguin Mass (g)"), 
            ui.output_text("pred_out")
        )
    )   
)

def server(input, output, session):
    logging.info("App start")

    @reactive.Calc
    def vals():
        d = {
            "bill_length_mm" : input.bill_length(),
            "sex_male" : input.sex() == "Male",
            "species_Gentoo" : input.species() == "Gentoo", 
            "species_Chinstrap" : input.species() == "Chinstrap"

        }
        return d
    
    @reactive.Calc
    @reactive.event(input.predict)
    def pred():
        logging.info("Request Made")
        r = requests.post(api_url, json = [vals()])
        logging.info("Request Returned")

        if r.status_code != 200:
            logging.error("HTTP error returned")

        return r.json().get('predict')[0]

    @output
    @render.text
    def vals_out():
        return f"{vals()}"

    @output
    @render.text
    def pred_out():
        return f"{round(pred())}"

app = App(app_ui, server)
