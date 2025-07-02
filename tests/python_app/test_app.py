import sys
from pathlib import Path
import pandas as pd
import types

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
import python_app.app as app


def test_load_csv(tmp_path):
    csv_content = 'a,b\n1,2\n3,4\n'
    file_path = tmp_path / 'data.csv'
    file_path.write_text(csv_content)
    with file_path.open('r') as f:
        df = app.load_csv(f)
    assert list(df.columns) == ['a', 'b']
    assert df.iloc[0]['a'] == 1
    assert df.iloc[1]['b'] == 4


def test_ask_question(monkeypatch):
    df = pd.DataFrame({'a': [1, 2]})
    captured = {}

    class DummyResp:
        class Choice:
            def __init__(self, content):
                self.message = types.SimpleNamespace(content=content)
        def __init__(self, content):
            self.choices = [self.Choice(content)]

    def fake_create(**kwargs):
        captured['prompt'] = kwargs
        return DummyResp('dummy answer')

    monkeypatch.setenv('OPENAI_API_KEY', 'x')
    monkeypatch.setattr(app.openai.ChatCompletion, 'create', fake_create)
    answer = app.ask_question(df, 'What is a?')
    assert 'dummy answer' == answer
    assert 'messages' in captured['prompt']

