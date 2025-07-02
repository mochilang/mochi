import os
import pandas as pd
import streamlit as st
import openai


def load_csv(file) -> pd.DataFrame:
    """Load a CSV file into a pandas DataFrame."""
    return pd.read_csv(file)


def ask_question(df: pd.DataFrame, question: str) -> str:
    """Use OpenAI to answer a question about the DataFrame."""
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        return "OPENAI_API_KEY not set"
    openai.api_key = api_key
    sample = df.head(20).to_csv(index=False)
    prompt = (
        "You are a data analyst. Given the following CSV data:\n" + sample +
        f"\nAnswer the following question about this data:\n{question}"
    )
    try:
        resp = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[{"role": "user", "content": prompt}],
        )
        return resp.choices[0].message.content.strip()
    except Exception as e:
        return f"Error from OpenAI: {e}"


def main():
    st.title("CSV Explorer")

    if "data_frames" not in st.session_state:
        st.session_state["data_frames"] = {}

    with st.form("upload_form"):
        uploaded_file = st.file_uploader("Upload CSV", type="csv")
        submitted = st.form_submit_button("Upload")
        if submitted and uploaded_file:
            df = load_csv(uploaded_file)
            st.session_state["data_frames"][uploaded_file.name] = df

    if not st.session_state["data_frames"]:
        st.info("Upload one or more CSV files to get started.")
        return

    tabs = st.tabs(list(st.session_state["data_frames"].keys()))

    for i, (name, df) in enumerate(st.session_state["data_frames"].items()):
        with tabs[i]:
            st.subheader(name)
            st.dataframe(df)
            key_prefix = name.replace(" ", "_")
            chat_key = f"chat_{key_prefix}"
            if chat_key not in st.session_state:
                st.session_state[chat_key] = []

            with st.form(f"chat_form_{key_prefix}"):
                question = st.text_input("Ask a question about this data")
                ask = st.form_submit_button("Ask")
                if ask and question:
                    answer = ask_question(df, question)
                    st.session_state[chat_key].append(("You", question))
                    st.session_state[chat_key].append(("Assistant", answer))

            for speaker, text in st.session_state[chat_key]:
                st.markdown(f"**{speaker}:** {text}")


if __name__ == "__main__":
    main()
