from src.ApiOutput import ApiOutput
from os import system

from datetime import datetime


class Convert:
    def __init__(self):
        self.apiOutput = ApiOutput()

    def __getProblemSnippet(self, snippets: dict, language: str) -> str:
        for e in snippets:
            if e['lang_slug'] == language:
                return e['code']

    def __convertProblemContent(self, content: str, title: str) -> str:
        currentDatetime = datetime.now().strftime("%H%M-%d%m%Y")
        filename = f"{currentDatetime}-{title}"
        with open(f'/tmp/{filename}.html', "w") as htmlFile:
            htmlFile.write(content)

        sedQuery = r"""
        s/<p>//g;
        s@<\/p>@@g;
        s@<pre>@\#+begin_src@g;
        s@</pre>@\#+end_src@g;
        s/<strong class.*Example/\*\* Example/g;
        s/:.*>//g;
        s@<em>@@g;
        s@</em>@@g;
        s/<strong>/\*/g;
        s@</strong>@\*@g;
        s/&nbsp\; //g;
        s/\*Constraints/\* Constraints/g;
        s/<code>/=/g;
        s@</code>@=@g;
        s@\*\* Example 1@* Examples\n\n** Example 1@g;
        s@&#39\;@"@g;
        s@&quot;@"@g;
        s/<sup>/^/g;
        s@</sup>@@g;
        s@&lt\;@<@g;
        s/<li>/- [ ] /g;
        s/[ \t]-/-/g
        s@.</li>@@g;
        s/+ Only/Only/;
        s@&quot\; @\"@g;
        s@\*Input@\n*Input@g;
        s@\&nbsp\;@@g;
        s@<p>@@g;
        s@\#+end_src@#+end_src\n@g;
        /ul>/d;
        """

        system(
            f"sed '{sedQuery}' /tmp/{filename}.html > /tmp/{filename}.org")

        with open(f'/tmp/{filename}.org', 'r') as file:
            output = file.read()

        system(f"rm /tmp/{filename}.*")

        return output

    def __getProblemValues(self, problemTitle: str, questionData: dict,
                           language: str) -> tuple[str]:

        questionId = questionData['question_id']
        difficulty = questionData['difficulty']
        likes = questionData['likes']
        dislikes = questionData['dislikes']
        content = questionData['content']
        content = self.__convertProblemContent(content, problemTitle)
        hints = questionData['hints']
        snippets = questionData['code_snippets']
        snippet = self.__getProblemSnippet(snippets, language)

        return questionId, difficulty, likes, dislikes, content, \
            hints, snippet

    def __getQuestionData(self, leetcodeSession: str, csrfToken: str,
                          problemTitle: str, debug: bool) -> dict:

        apiOutput = self.apiOutput.getApiOutput(leetcodeSession, csrfToken,
                                                problemTitle, debug)
        questionData = apiOutput['data']['question']

        return questionData

    def getConvertedProblem(self, leetcodeSession: str, csrfToken: str,
                            debug: str, problemTitle: str, language: str
                            ) -> str:

        questionData = self.__getQuestionData(leetcodeSession, csrfToken,
                                              problemTitle, debug)

        questionId, difficulty, likes, dislikes, content, hints, snippet = \
            self.__getProblemValues(problemTitle, questionData, language)

        convertedProblem = f"""#+TITLE: {problemTitle}

* Information
It is ={questionId}= question, it ={difficulty.lower()}= problem.
={likes}= liked and ={dislikes}= disliked.

* The problem
{content}
* Additional info
You need to use this code as base
#+begin_src {language}
{snippet}
#+end_src

{''.join(hints)}
"""

        return convertedProblem
