import streamlit as st
import plotly.express as px
import pandas as pd





def main():
    df_lucro = pd.read_csv('data/df_lucro_prejuizo.zip')
    df_lucro['data'] = pd.to_datetime(df_lucro['data'])

    df_acao = pd.read_csv('data/stocks_prices_trading.csv')
    df_keys = pd.read_csv('data/carteira_ibov_keys.csv')

    st.title('Mercado de Ações')

    # Meny companies options
    empresa = st.sidebar.selectbox('Empresa', df_keys['nome_empresarial'])
    # Company cvm code
    codigo_cvm = str(df_keys[df_keys['nome_empresarial'] == empresa]['codigo_cvm'].values[0]).zfill(6)
    # Company negotiation code
    codigo_negociacao = df_keys[df_keys['nome_empresarial'] == empresa]['index']

    # Remove years without data
    df_lucro_clean = df_lucro[['data', codigo_cvm]].dropna()
    # Plot
    # Figure
    st.plotly_chart(px.line(df_lucro_clean, x='data', y=codigo_cvm, title=f'Lucro Prejuizo {empresa} (1 MIL)'))


if __name__ == '__main__':
    main()
