import leetcode.configuration as x
import leetcode.auth
import leetcode


class ApiOutput:
    @staticmethod
    def __getApiInstance(leetcodeSession: str, csrfToken: str,
                         debug: bool = False) -> x.Configuration:
        configuration = leetcode.Configuration()
        configuration.api_key["x-csrftoken"] = csrfToken
        configuration.api_key["csrftoken"] = csrfToken
        configuration.api_key["LEETCODE_SESSION"] = leetcodeSession
        configuration.api_key["Referer"] = "https://leetcode.com"
        configuration.debug = debug

        apiInstance = leetcode.DefaultApi(leetcode.ApiClient(configuration))

        return apiInstance

    def getApiOutput(self, leetcodeSession: str, csrfToken: str,
                     titleSlug: str, debug: bool = False) -> dict:
        graphql_request = leetcode.GraphqlQuery(
            query="""
                query getQuestionDetail($titleSlug: String!) {
                question(titleSlug: $titleSlug) {
                    title
                    difficulty
                    likes
                    dislikes
                    content
                    questionId
                    similarQuestions
                    topicTags {
                    name
                    slug
                    translatedName
                    __typename
                    }
                    companyTagStats
                    codeSnippets {
                    lang
                    langSlug
                    code
                    __typename
                    }
                    stats
                    hints
                    sampleTestCase
                    judgerAvailable
                    judgeType
                    enableTestMode
                    __typename
                }
                }
            """,
            variables=leetcode.GraphqlQueryGetQuestionDetailVariables(
                title_slug=titleSlug),
            operation_name="getQuestionDetail")

        apiInstance = self.__getApiInstance(leetcodeSession, csrfToken, debug)
        apiOutput = apiInstance.graphql_post(body=graphql_request).to_dict()

        return apiOutput
