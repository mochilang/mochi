package leetcode

import (
	"bytes"
	"encoding/json"
	"fmt"
	"html"
	"io"
	"net/http"
	"os"
	"strings"
)

// Download retrieves the LeetCode problem with the given numeric ID.
// It returns the plain text content of the problem without HTML tags.
func Download(id int) (string, error) {
	// Step 1: search for the problem to get titleSlug
	searchBody := map[string]any{
		"operationName": "problemsetQuestionList",
		"variables": map[string]any{
			"categorySlug": "",
			"skip":         0,
			"limit":        1,
			"filters": map[string]any{
				"searchKeywords": fmt.Sprint(id),
			},
		},
		"query": `query problemsetQuestionList($categorySlug: String, $limit: Int, $skip: Int, $filters: QuestionListFilterInput) { problemsetQuestionList: questionList(categorySlug: $categorySlug, limit: $limit, skip: $skip, filters: $filters) { total: totalNum questions: data { frontendQuestionId: questionFrontendId title titleSlug difficulty } } }`,
	}
	data, err := json.Marshal(searchBody)
	if err != nil {
		return "", err
	}
	resp, err := http.Post("https://leetcode.com/graphql/", "application/json", bytes.NewReader(data))
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	rdata, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	var searchResp struct {
		Data struct {
			ProblemsetQuestionList struct {
				Questions []struct {
					TitleSlug string `json:"titleSlug"`
				} `json:"questions"`
			} `json:"problemsetQuestionList"`
		} `json:"data"`
	}
	if err := json.Unmarshal(rdata, &searchResp); err != nil {
		return "", err
	}
	if len(searchResp.Data.ProblemsetQuestionList.Questions) == 0 {
		return "", fmt.Errorf("problem %d not found", id)
	}
	slug := searchResp.Data.ProblemsetQuestionList.Questions[0].TitleSlug
	// Step 2: get full problem content
	contentBody := map[string]any{
		"operationName": "questionData",
		"variables":     map[string]any{"titleSlug": slug},
		"query":         `query questionData($titleSlug: String!) { question(titleSlug: $titleSlug) { questionId title content difficulty likes dislikes } }`,
	}
	data, err = json.Marshal(contentBody)
	if err != nil {
		return "", err
	}
	req, err := http.NewRequest("POST", "https://leetcode.com/graphql/", bytes.NewReader(data))
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/json")
	if session := os.Getenv("LEETCODE_SESSION"); session != "" {
		req.Header.Set("Cookie", "LEETCODE_SESSION="+session)
	}
	resp, err = http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	rdata, err = io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}
	var contentResp struct {
		Data struct {
			Question struct {
				Content string `json:"content"`
			} `json:"question"`
		} `json:"data"`
	}
	if err := json.Unmarshal(rdata, &contentResp); err != nil {
		return "", err
	}
	text := removeHTMLTags(contentResp.Data.Question.Content)
	return text, nil
}

func removeHTMLTags(s string) string {
	var b strings.Builder
	inTag := false
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '<':
			inTag = true
		case '>':
			if inTag {
				inTag = false
			} else {
				b.WriteByte('>')
			}
		default:
			if !inTag {
				b.WriteByte(s[i])
			}
		}
	}
	return html.UnescapeString(b.String())
}
