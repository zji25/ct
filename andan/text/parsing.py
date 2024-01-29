from googleapiclient.discovery import build
import csv


def get_video_comments(video_link):
    api_key = 'AIzaSyDQIGGMoK6jvCFHOL8mRSXNfqcg-5HvZCU'
    video_id = video_link.split('?v=')[1].split('&')[0]
    youtube = build('youtube', 'v3', developerKey=api_key)
    comments = []
    next_page_token = None
    while True:
        response = youtube.commentThreads().list(
            part='snippet',
            videoId=video_id,
            textFormat='plainText',
            maxResults=100,
            pageToken=next_page_token
        ).execute()
        for item in response['items']:
            comment = item['snippet']['topLevelComment']['snippet']
            comments.append([comment['textDisplay'], comment['likeCount'], comment.get('dislikeCount', 0)])
        if 'nextPageToken' in response:
            next_page_token = response['nextPageToken']
        else:
            break
    return comments


def save_comments_to_csv(comments, filename):
    with open(filename, 'w', newline='', encoding='utf-8') as csvfile:
        csv_writer = csv.writer(csvfile)
        csv_writer.writerow(['comment', 'likes', 'dislikes'])
        csv_writer.writerows(comments)


def main():
    video_link = 'https://www.youtube.com/watch?v=IIuRc8IiSuE&ab_channel=PewDiePie'
    comments = get_video_comments(video_link)
    save_comments_to_csv(comments, 'output/comments.csv')


main()
