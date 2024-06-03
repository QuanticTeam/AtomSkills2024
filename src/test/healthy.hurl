
GET https://localhost:5004/login
HTTP/1.1 200
[Asserts]
certificate "Subject" == "CN = localhost"
xpath "string(/html/head/title)" contains "Welding and Sons - AtomSkills2024"

POST https://localhost:5003/Something/Test
HTTP/2 200
Content-Type: text/plain; charset=utf-8 
[Asserts]
body equals "Test"
