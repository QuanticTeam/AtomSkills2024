import { TagFilled, UserOutlined } from '@ant-design/icons'
import { useQuery } from '@tanstack/react-query'
import { List, Space, Spin, Tabs, Tag, Typography } from 'antd'
import { useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useParams } from 'react-router-dom'
import { Lesson, LessonsApi, Task } from '~/entities'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { Markdown, Oops } from '~/shared/ui'
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
              <Space>
                {data?.traits.map(x => (
                  <Tag
                    key={(x as any).code}
                    color="orange-inverse"
                  >
                    <TagFilled /> {(x as any).name}
                  </Tag>
                ))}
              </Space>
              <Typography.Text
                type="secondary"
                className="ml-6"
              >
                <UserOutlined /> {data?.author}
              </Typography.Text>
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
                    {/* <Typography.Paragraph>
                      <Typography.Text>{data.content}</Typography.Text>
                    </Typography.Paragraph> */}
                    <Markdown markdown={data.content} />
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
