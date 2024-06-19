import {
  ClockCircleFilled,
  ClockCircleOutlined,
  DashboardOutlined,
  DeleteFilled,
  DeleteOutlined,
  InboxOutlined,
  StarOutlined,
  StarTwoTone,
} from '@ant-design/icons'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  Badge,
  Button,
  Card,
  FloatButton,
  Form,
  Input,
  List,
  Modal,
  Space,
  Spin,
  Tabs,
  Tag,
  Typography,
  Upload,
} from 'antd'
import TextArea from 'antd/es/input/TextArea'
import { t } from 'i18next'
import { memo, useCallback, useContext, useEffect, useRef, useState } from 'react'
import { Controller, SubmitHandler, useFieldArray, useForm } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { useParams } from 'react-router-dom'
import { string } from 'zod'
import {
  Attachment,
  AutomationSystemStatus,
  Lesson,
  SomethingApi,
  Task,
  TaskStatusType,
  TasksApi,
} from '~/entities'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { getIsDetailedApiError } from '~/shared/apiClient'
import { AuthContext } from '~/shared/auth'
import { Condition, Markdown, Oops } from '~/shared/ui'
import { Attachments } from '~/widgets/Attachments'

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
  const [isModalOpen, setIsModalOpen] = useState(false)
  const [isModalOpen2, setIsModalOpen2] = useState(false)

  const { data, error, isPending } = useQuery({
    queryKey: ['task', taskCode],
    queryFn: () => TasksApi.getOne(taskCode!),
  })

  const queryClient = useQueryClient()

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

  const willEndAt = Date.parse(currentTaskProgress.startedAt) + data.time * 60 * 1000
  const willEndIn = willEndAt - Date.now()

  const showModal = () => {
    setIsModalOpen(true)
  }

  const handleOk = async (fileKeyAndDescriptions: FormSubmitValues['test']) => {
    setIsModalOpen(false)
    await TasksApi.submit({
      taskStatusId: currentTaskProgress.id,
      fileKeyAndDescriptions,
    })

    await queryClient.fetchQuery({
      queryKey: ['task', taskCode],
    })
  }

  const handleCancel = () => {
    setIsModalOpen(false)
  }

  const showModal2 = () => {
    setIsModalOpen2(true)
  }

  const handleOk2 = async () => {
    setIsModalOpen2(false)
    await TasksApi.review({
      taskId: currentTaskProgress.id,
      mark: 4,
      defects: [
        {
          fileKey: 'DEFECTO!',
          codes: [],
          comment: 'bla',
          x1: 0,
          y1: 0,
          x2: 10,
          y2: 10,
        },
      ],
    })

    await queryClient.fetchQuery({
      queryKey: ['task', taskCode],
    })
  }

  const handleCancel2 = () => {
    setIsModalOpen2(false)
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
                    () =>
                      (isStudentOnLookup || isAdmin) &&
                      [TaskStatusType.Recommended].includes(currentTaskProgress.status),
                  ]}
                >
                  <Button
                    type="primary"
                    className="my-2"
                    onClick={async () => {
                      try {
                        await TasksApi.begin({ code: taskCode! })
                        await queryClient.fetchQuery({
                          queryKey: ['task', taskCode],
                        })
                      } catch (e) {
                        console.error(e)
                      }
                    }}
                  >
                    Взять в работу
                  </Button>
                </Condition>
                <Condition
                  every
                  conditions={[
                    () =>
                      (isAdmin || isStudentOnLookup) &&
                      currentTaskProgress.status === TaskStatusType.InWork,
                    () =>
                      currentTaskProgress.status !== TaskStatusType.AiVerified &&
                      currentTaskProgress.status !== TaskStatusType.Verified,
                  ]}
                >
                  <Button
                    type="primary"
                    className="!bg-primary-2 !border-primary-2 my-2"
                    onClick={showModal}
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
                    type="primary"
                    className="!bg-primary-2 !border-primary-2 my-2"
                    onClick={showModal2}
                  >
                    {currentTaskProgress.status === TaskStatusType.AiVerified
                      ? 'Уточнить'
                      : 'Проверить'}
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
                    type="primary"
                    className="!bg-primary-2 !border-primary-2 my-2"
                    onClick={async () => {
                      await TasksApi.suggestRetry(currentTaskProgress.id)
                      await queryClient.fetchQuery({
                        queryKey: ['task', taskCode],
                      })
                    }}
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
                        () =>
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          currentTaskProgress.status === TaskStatusType.InWork &&
                          willEndAt < Date.now(),
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
                        () =>
                          // Если бэк не успел посчитать, нарисуем пока сами
                          (isStudentOnLookup || isAdmin || isMentorOnReview) &&
                          currentTaskProgress.status === TaskStatusType.InWork &&
                          willEndAt >= Date.now(),
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
                          [TaskStatusType.Verified, TaskStatusType.AiVerified].includes(
                            currentTaskProgress.status,
                          ),
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
                          [TaskStatusType.Recommended].includes(currentTaskProgress.status),
                      ]}
                    >
                      <Badge
                        status="warning"
                        text="Рекомендация перепройти"
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
                        {Array.from({ length: 5 }).map((_, i) => {
                          if (!currentTaskProgress.mark)
                            return <StarOutlined className="text-slate-400" />
                          return i + 1 <= currentTaskProgress.mark ? (
                            <StarTwoTone />
                          ) : (
                            <StarOutlined className="text-slate-400" />
                          )
                        })}
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
          {
            <Condition
              conditions={[
                () =>
                  (isAdmin || isStudentOnLookup) &&
                  currentTaskProgress.status === TaskStatusType.InWork,
              ]}
            >
              <Timer
                value={willEndIn}
                onFinish={() =>
                  queryClient.fetchQuery({
                    queryKey: ['task', taskCode],
                  })
                }
              />
            </Condition>
          }
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
                children: <Attachments fileKeys={data.supplements} />,
              },
            ]}
            onChange={console.log.bind(console)}
          />

          <ModalSubmitTask
            isModalOpen={isModalOpen}
            handleOk={handleOk}
            handleCancel={handleCancel}
          />

          <Modal
            title="Проверка решения"
            open={isModalOpen2}
            onOk={handleOk2}
            onCancel={handleCancel2}
          >
            <p>Some contents...</p>
            <p>Some contents...</p>
            <p>Some contents...</p>
          </Modal>
        </>
      )}
    </PageAuthorized>
  )
}

interface TimerProps {
  value: number
  onFinish: () => void
}

const Timer = memo(({ value, onFinish }: TimerProps) => {
  const [timerValue, setTimerValue] = useState(value)
  const intervalRef = useRef<NodeJS.Timer | null>(null)

  useEffect(() => {
    intervalRef.current = setInterval(() => {
      setTimerValue(prev => prev - 1000)
    }, 1000)

    return () => clearInterval(intervalRef.current as unknown as number)
  }, [])

  if (Date.now() + timerValue <= Date.now()) {
    clearInterval(intervalRef.current as unknown as number)
    onFinish()
  }

  console.log(timerValue)

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
          {mins < 10 ? '0' + mins : mins}:{secs < 10 ? '0' + secs : secs}
        </Tag>
      }
    />
  )
})

interface FormSubmitValues {
  test: {
    fileKey: string
    description?: string
  }[]
}

function ModalSubmitTask({ isModalOpen, handleOk, handleCancel }: Record<string, any>) {
  const {
    control,
    formState: { errors },
    handleSubmit,
    setValue,
    register,
  } = useForm<FormSubmitValues>({
    mode: 'onBlur',
    values: {
      test: [],
    },
  })

  const { fields, append, prepend, remove, swap, move, insert } = useFieldArray({
    name: 'test',
    control,
  })

  const [submissionError, setSubmissionError] = useState('')

  const appendInputGroup = useCallback(
    () =>
      append({
        fileKey: '',
        description: '',
      }),
    [append],
  )

  return (
    <Modal
      title="Отправить решение"
      open={isModalOpen}
      footer={
        <div className="flex justify-between">
          <Button
            type="primary"
            ghost
            onClick={() => appendInputGroup()}
          >
            Добавить файл
          </Button>

          <Space size="middle">
            <Button
              type="primary"
              ghost
              onClick={handleCancel}
            >
              Отменить
            </Button>
            <Button
              form="myform"
              type="primary"
              htmlType="submit"
            >
              Отправить
            </Button>
          </Space>
        </div>
      }
    >
      <Form
        id="myform"
        onFinish={async values => {
          try {
            await handleSubmit(handleOk)(values)
          } catch (error) {
            if (!getIsDetailedApiError(error)) throw error

            switch (error.code) {
              default: {
                setSubmissionError(error.code)
              }
            }
          }
        }}
      >
        {fields.map((field, index) => (
          <Card
            title={
              <div className="flex justify-between items-center">
                <Typography.Text strong>Вложение {index + 1}</Typography.Text>
                <Typography.Link
                  type="danger"
                  onClick={() => remove(index)}
                >
                  <DeleteOutlined />
                </Typography.Link>
              </div>
            }
            size="small"
          >
            <Form.Item
              className="!mb-0"
              key={field.id}
            >
              <Upload.Dragger
                // This handler is invoked per file even in case of batch upload
                customRequest={async ({ file, onProgress, onSuccess, onError }) => {
                  try {
                    const result = await SomethingApi.upload(file, { onProgress })
                    setValue(`test.${index}.fileKey`, result.fileKey)

                    onSuccess?.(result)
                  } catch (error: any) {
                    onError?.(error) // TODO handle properly
                  }
                }}
              >
                <div className="flex items-center">
                  <div className="text-4xl text-blue-500 mr-2">
                    <InboxOutlined />
                  </div>
                  <Typography.Text className="text-sm text-blue-500">
                    Кликните для загрузки либо перетяните файл из проводника
                  </Typography.Text>
                </div>
              </Upload.Dragger>
              <Controller
                control={control}
                name={`test.${index}.description`}
                render={({ field }) => (
                  <Input
                    {...field}
                    className="mt-4"
                    placeholder="Комментарий (необязательно)"
                  />
                )}
              />
            </Form.Item>
          </Card>
        ))}
      </Form>
    </Modal>
  )
}
