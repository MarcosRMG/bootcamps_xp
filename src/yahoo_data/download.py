import streamlit as st
from datetime import date
import yfinance as yf


@st.cache_data
def request_data(selected_tickers: list, start_date: str, end_date: str = None):
    """
    Download historical financial data from Yahoo Finance about ticker negotiation

    Parameters: selected_tickers : List of string
                    Company tickers selected to download

                start_date: String
                    Initial date to download historical data

    Returns: DataFrame
                Pandas DataFrame with Open, High, Low, Close, Adj Close and Volume columns about selected tickers
                market negotiation
    """
    today = date.today()
    if end_date:
        df = yf.download(tickers=selected_tickers, start=start_date, end=end_date)
    else:
        df = yf.download(tickers=selected_tickers, start=start_date, end=today)
    return df