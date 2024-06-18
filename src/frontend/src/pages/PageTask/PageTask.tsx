import {
  ClockCircleFilled,
  ClockCircleOutlined,
  DashboardOutlined,
  StarFilled,
  StarOutlined,
  StarTwoTone,
} from '@ant-design/icons'
import { useQuery } from '@tanstack/react-query'
import { Badge, Button, Card, FloatButton, List, Space, Spin, Tabs, Tag, Typography } from 'antd'
import { memo, useContext, useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { useParams } from 'react-router-dom'
import {
  AutomationSystemStatus,
  Lesson,
  Task,
  TaskProgress,
  TaskStatusType,
  TasksApi,
} from '~/entities'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'
import { AuthContext } from '~/shared/auth'
import { UserRoleEnumStr } from '~/shared/auth/UserRoleEnum'
import { colors } from '~/shared/styles'
import { Condition, Markdown, Oops } from '~/shared/ui'

interface PageTaskRouteParams {
  lessonCode: Lesson['code']
  taskCode: Task['code']
  [x: string]: string | undefined
}

export function PageTask() {
  const { lessonCode, taskCode, userId: _userId } = useParams<PageTaskRouteParams>()
  const { t } = useTranslation(PageTask.name)
  const [content, setContent] = useState('')
  const { authInfo } = useContext(AuthContext)

  const { data, error, isPending } = useQuery({
    queryKey: ['task', taskCode],
    queryFn: () => TasksApi.getOne(taskCode!),
  })

  if (isPending) return <Spin fullscreen />
  if (error) return <Oops />

  const isMentorOnReview = authInfo?.user.isMentor && _userId
  const isMentorOnLookup = authInfo?.user.isMentor && !_userId

  const isStudentOnLookup = authInfo?.user.isStudent
  const isAdmin = authInfo?.user.isAdmin

  const userId = isMentorOnReview ? _userId : authInfo?.user.userId

  const everyoneProgress = data.taskStatuses

  const progress = everyoneProgress
    .filter(p => p.userKey === userId)
    .sort((a, b) => Date.parse(b.startedAt) - Date.parse(a.startedAt))

  const currentTaskProgress = progress[0] ?? {
    id: 0,
    status: TaskStatusType.None,
    automationSystemStatus: AutomationSystemStatus.None,
    startedAt: '',
    finishedAt: '',
    mark: 0,
    fotos: [],
    userKey: '',
    taskCode,
    recommendations: [],
    defects: [],
  }

  return (
    <PageAuthorized
      title={
        <>
          <div className="flex justify-between items-center mb-4">
            <Typography.Title className="!mb-0">Задание</Typography.Title>
          </div>
          <Card
            className="mb-4"
            title={
              <Typography.Text
                strong
                className="text-lg"
              >
                {data?.title ?? ''}
              </Typography.Text>
            }
            size="small"
            extra={
              <Space>
                <Condition
                  conditions={[
                    () =>
                      (isAdmin || isStudentOnLookup) &&
                      currentTaskProgress.status === TaskStatusType.None,
                  ]}
                >
                  <Button
                    size="middle"
                    type="primary"
                    className="my-2"
                    onClick={async () => TasksApi.begin({ code: taskCode! })}
                  >
                    Взять в работу
                  </Button>
                </Condition>
                <Condition
                  conditions={[
                    () =>
                      isAdmin &&
                      isStudentOnLookup &&
                      currentTaskProgress.status === TaskStatusType.InWork,
                  ]}
                >
                  <Button
                    size="small"
                    type="primary"
                    className="!bg-primary-2 !border-primary-2"
                  >
                    Отправить на проверку
                  </Button>
                </Condition>
                <Condition
                  conditions={[
                    () =>
                      (isAdmin || isMentorOnReview) &&
                      [TaskStatusType.SendToCheck, TaskStatusType.AiVerified].includes(
                        currentTaskProgress.status,
                      ),
                  ]}
                >
                  <Button
                    size="small"
                    type="primary"
                    className="!bg-primary-2 !border-primary-2"
                  >
                    Проверить
                  </Button>
                </Condition>
                <Condition
                  conditions={[
                    () =>
                      (isAdmin || isMentorOnReview) &&
                      currentTaskProgress.status === TaskStatusType.Verified,
                  ]}
                >
                  <Button
                    size="small"
                    type="primary"
                    className="!bg-primary-2 !border-primary-2"
                  >
                    Предложить перепройти
                  </Button>
                </Condition>
              </Space>
            }
          >
            <div className="flex justify-between">
              <Space size="middle">
                <Condition conditions={[() => isAdmin || isMentorOnReview || isStudentOnLookup]}>
                  <Space size="middle">
                    <Condition
                      conditions={[
                        () =>
                          isStudentOnLookup && currentTaskProgress.status === TaskStatusType.None,
                      ]}
                    >
                      <Badge
                        status="default"
                        text="Новое"
                      />
                    </Condition>
                    <Condition
                      conditions={[
                        () =>
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          currentTaskProgress.status === TaskStatusType.InWork,
                      ]}
                    >
                      <Badge
                        status="processing"
                        text="Выполняется"
                      />
                    </Condition>
                    <Condition
                      conditions={[
                        () =>
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          currentTaskProgress.status === TaskStatusType.SendToCheck,
                      ]}
                    >
                      <Badge
                        status="processing"
                        color="orange"
                        text="На проверке"
                      />
                    </Condition>
                    <Condition
                      conditions={[
                        () =>
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          currentTaskProgress.status === TaskStatusType.AiVerified,
                      ]}
                    >
                      <Badge
                        status="success"
                        text="Проверено AI"
                      />
                    </Condition>
                    <Condition
                      conditions={[
                        () =>
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          currentTaskProgress.status === TaskStatusType.Verified,
                      ]}
                    >
                      <Badge
                        status="success"
                        text="Проверено"
                      />
                    </Condition>
                    <Condition
                      conditions={[
                        () =>
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          [TaskStatusType.Verified, TaskStatusType.AiVerified].includes(
                            currentTaskProgress.status,
                          ),
                      ]}
                    >
                      <Space className="relative top-px">
                        <StarTwoTone />
                        <StarTwoTone />
                        <StarTwoTone />
                        <StarOutlined className="text-slate-400" />
                        <StarOutlined className="text-slate-400" />
                      </Space>
                    </Condition>
                  </Space>
                </Condition>
                <Typography.Text strong>
                  <DashboardOutlined /> {data?.difficulty}
                </Typography.Text>
                <Typography.Text>
                  <ClockCircleOutlined /> {data?.time}мин
                </Typography.Text>
              </Space>
            </div>
          </Card>
        </>
      }
    >
      {error ? (
        <Oops />
      ) : (
        <>
          <CountDown />
          <Tabs
            size="small"
            defaultActiveKey="1"
            items={[
              {
                key: 'content',
                label: t('tabContent'),
                children: <Markdown markdown={data.content} />,
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

const CountDown = memo(() => {
  const [timerValue, setTimerValue] = useState(Date.now() + 1000 * 60 * 5)

  useEffect(() => {
    setInterval(() => {
      setTimerValue(prev => prev - 1000)
    }, 1000)
  }, [])

  const d = new Date(timerValue)
  const mins = d.getMinutes()
  const secs = d.getSeconds()

  return (
    <FloatButton
      shape="square"
      icon={<ClockCircleFilled />}
      style={{
        right: '60px',
        top: '150px',
        bottom: 'unset',
      }}
      description={
        <Tag className="absolute translate-x-1/2 top-11 right-1/2 m-0 text-base">
          {mins}:{secs < 10 ? '0' + secs : secs}
        </Tag>
      }
    />
  )
})
