import streamlit as st
import plotly.express as px
import plotly.graph_objects as go
import pandas as pd
from yahoo_data.download import request_data
from datetime import datetime, timedelta
import ta

# Settings
st.set_page_config(layout='wide')


def main():
    # ==================== Data input ====================
    df_lucro = pd.read_csv('data/df_lucro_prejuizo.zip')
    df_keys = pd.read_csv('data/carteira_ibov_keys.csv')

    # ===================== Variables =====================
    # Meny companies options
    empresa = st.sidebar.selectbox('Empresa', df_keys['nome_empresarial'])
    # Company cvm code
    codigo_cvm = str(df_keys[df_keys['nome_empresarial'] == empresa]['codigo_cvm'].values[0]).zfill(6)
    # Company negotiation code
    codigo_negociacao = df_keys[df_keys['nome_empresarial'] == empresa]['index'].values[0] + '.SA'

    # ===================== Data preprocessing ====================
    # Remove years without data
    df_lucro_clean = df_lucro[['data', codigo_cvm]].dropna().drop_duplicates()
    # Negotiation
    start_date = datetime.today() - timedelta(days=365 * 2)
    df_stocks = request_data(codigo_negociacao, start_date)

    # ==================== Feature engineering ====================
    # Lucro/ prejuízo
    # Crescimento percentual
    df_lucro_clean['crescimento_pct'] = df_lucro_clean[codigo_cvm].pct_change(1)

    # Negociação
    # Média móvel
    df_stocks['close_mean'] = df_stocks['Close'].rolling(30).mean()

    # Volume Weighted Average Price (VWAP)
    RSI = ta.momentum.RSIIndicator(df_stocks['Close'], window=5)

    df_stocks['RSI'] = RSI.rsi()

    # ==================== Data Viz ====================
    st.title('Mercado de Ações')
    col1, col2 = st.columns([0.5, 0.5])
    # Column 1
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=df_stocks.index, y=df_stocks['Close'],
                             mode='lines',
                             name='Fechamento'))
    fig.add_trace(go.Scatter(x=df_stocks.index, y=df_stocks['close_mean'],
                             mode='lines',
                             name='Fechamento Médio'))

    fig.update_layout(title=f'Preço de Fechamento {empresa}',
                      yaxis_title='R$',
                      xaxis_title='Data')
    col1.plotly_chart(fig)

    fig = px.line(df_stocks, y='RSI', title=f'RSI {empresa}')
    col1.plotly_chart(fig)

    # Column 2
    fig1 = px.line(df_lucro_clean, x='data', y=codigo_cvm, title=f'Lucro/ Prejuizo {empresa} (1 MIL)')
    fig1.update_layout(yaxis_title='R$', xaxis_title='Data')
    col2.plotly_chart(fig1)

    fig2 = px.line(df_lucro_clean, x='data', y='crescimento_pct', title=f'Lucro/ Prejuízo {empresa} %')
    fig2.update_layout(yaxis_title='%', xaxis_title='Data')
    col2.plotly_chart(fig2)


if __name__ == '__main__':
    main()
