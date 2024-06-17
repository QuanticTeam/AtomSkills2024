import { useQuery } from '@tanstack/react-query'
import { List, Space, Spin, Tabs, Tag, Typography } from 'antd'
import { useState } from 'react'
import { useTranslation } from 'react-i18next'
import Markdown from 'react-markdown'
import { useParams } from 'react-router-dom'
import remarkGfm from 'remark-gfm'
import { Lesson, LessonsApi, Task } from '~/entities'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { Oops } from '~/shared/ui'
import { Tasks } from '~/widgets'

type PageLessonRouteParams = Pick<Lesson, 'code'>

export function PageLesson() {
  const { code } = useParams<PageLessonRouteParams>()
  const { t } = useTranslation(PageLesson.name)
  const [content, setContent] = useState('')

  const { data, error, isPending } = useQuery({
    queryKey: ['lesson', code],
    queryFn: () => LessonsApi.getOne(code!),
  })

  if (isPending) return <Spin fullscreen />

  return (
    <PageAuthorized
      title={
        <>
          <div className="flex justify-between items-center mb-6">
            <Typography.Title className="!mb-0">Учебный материал</Typography.Title>
            <div>
              <Space size="large">
                <div>
                  <Typography.Text type="secondary">{t('tags')}: </Typography.Text>
                  {data?.traits.map(x => (
                    <Tag
                      key={(x as any).code}
                      color="orange-inverse"
                    >
                      {(x as any).name}{' '}
                    </Tag>
                  ))}
                </div>
                <Typography.Text type="secondary">
                  {t('author')}: {data?.author}
                </Typography.Text>
              </Space>
            </div>
          </div>
          <Typography.Title level={2}>{data?.title ?? ''}</Typography.Title>
        </>
      }
    >
      {error ? (
        <Oops />
      ) : (
        <>
          <Tabs
            size="small"
            defaultActiveKey="1"
            items={[
              {
                key: 'content',
                label: t('tabContent'),
                children: (
                  <>
                    <Typography.Paragraph>
                      <Typography.Text>{data.content}</Typography.Text>
                    </Typography.Paragraph>
                    <Markdown
                      className="markdown-renderer"
                      components={{
                        h1: props => (
                          <Typography.Title level={1}>{props.children}</Typography.Title>
                        ),
                        h2: props => (
                          <Typography.Title level={2}>{props.children}</Typography.Title>
                        ),
                        h3: props => (
                          <Typography.Title level={3}>{props.children}</Typography.Title>
                        ),
                        h4: props => (
                          <Typography.Title level={4}>{props.children}</Typography.Title>
                        ),
                        h5: props => (
                          <Typography.Title level={5}>{props.children}</Typography.Title>
                        ),
                        li: props => <List.Item>{props.children}</List.Item>,
                        ol: props => (
                          <ol
                            {...props}
                            className="markdown-renderer-ol"
                          />
                        ),
                        p: props => <Typography.Paragraph>{props.children}</Typography.Paragraph>,
                        em: props => <Typography.Text italic>{props.children}</Typography.Text>,
                        strong: props => <Typography.Text strong>{props.children}</Typography.Text>,
                        blockquote: props => (
                          <Typography.Text type="secondary">{props.children}</Typography.Text>
                        ),
                      }}
                      remarkPlugins={[remarkGfm]}
                    >
                      {getMarkdown()}
                    </Markdown>
                  </>
                ),
              },
              {
                key: 'supplements',
                label: t('tabSupplements'),
                children: (
                  <List>
                    {data.supplements.map((s, i) => (
                      <List.Item key={i}>{JSON.stringify(s)}</List.Item>
                    ))}
                  </List>
                ),
              },
              {
                key: 'tasks',
                label: t('tabTasks'),
                children: (
                  <Tasks
                    lessonCode={code as string}
                    tasks={data.tasks as any as Task[]}
                  />
                ),
              },
            ]}
            onChange={console.log.bind(console)}
          />
        </>
      )}
    </PageAuthorized>
  )
}

function getMarkdown() {
  return `
  # h1 Heading 8-)
  ## h2 Heading
  ### h3 Heading
  #### h4 Heading
  ##### h5 Heading

  I just love <strong>bold text</strong>

  I just love **bold text**

  I just love <em>italic text</em>

  I just love *italic text*

  Unordered

  + Create a list by starting a line with \`+\`, \`-\`, or \`*\`
  + Sub-lists are made by indenting 2 spaces:
    - Marker character change forces new list start:
      * Ac tristique libero volutpat at
      + Facilisis in pretium nisl aliquet
      - Nulla volutpat aliquam velit
  + Very easy!

  Ordered

  1. Lorem ipsum dolor sit amet
  2. Consectetur adipiscing elit
  3. Integer molestie lorem at massa
  `
}
