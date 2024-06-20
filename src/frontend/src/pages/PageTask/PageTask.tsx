import {
  ArrowLeftOutlined,
  ArrowRightOutlined,
  ClockCircleFilled,
  ClockCircleOutlined,
  DashboardOutlined,
  DeleteFilled,
  DeleteOutlined,
  InboxOutlined,
  QuestionCircleOutlined,
  StarOutlined,
  StarTwoTone,
  ThunderboltOutlined,
} from '@ant-design/icons'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import {
  Badge,
  Button,
  Card,
  Carousel,
  Divider,
  Flex,
  FloatButton,
  Form,
  Image,
  Input,
  List,
  Modal,
  Popover,
  Rate,
  Space,
  Spin,
  Tabs,
  Tag,
  Tooltip,
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
import { TaskProgressList } from '~/widgets/TaskProgressList'

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

  const handleOk = async ({ test: fileKeyAndDescriptions }: { test: FormSubmitValues['test'] }) => {
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

  const handleOk2 = async ({ taskId, mark }: { taskId: number; mark: number }) => {
    setIsModalOpen2(false)
    await TasksApi.review({
      taskId,
      mark,
      defects: [],
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
            <Typography.Title className="!mb-0">
              {authInfo?.user.isMentor && _userId ? 'Проверка задания' : 'Задание'} (код {taskCode})
            </Typography.Title>
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
                          willEndAt <= Date.now(),
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
                          TaskStatusType.Verified === currentTaskProgress.status,
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
                <Typography.Text
                  strong
                  className="text-primary-2"
                >
                  <ThunderboltOutlined />{' '}
                  <span className="relative right-px ">{data?.difficulty}</span>
                </Typography.Text>
                <Typography.Text className="text-primary-2">
                  <ClockCircleOutlined /> <span className="relative left-px">{data?.time}мин</span>
                </Typography.Text>
              </Space>
              {isMentorOnReview && _userId ? `Студент: ${userId}` : ''}
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
                  currentTaskProgress.status === TaskStatusType.InWork &&
                  willEndAt >= Date.now(),
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
              {
                key: 'progress',
                label: 'Прохождение задания',
                children: <TaskProgressList />,
              },
            ]}
            onChange={console.log.bind(console)}
          />

          <ModalSubmitTask
            isModalOpen={isModalOpen}
            handleOk={handleOk}
            handleCancel={handleCancel}
          />

          <ModalFinalBoss
            data={currentTaskProgress}
            isModalOpen={isModalOpen2}
            handleOk={handleOk2}
            handleCancel={handleCancel2}
          />
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
      console.log([Date.now() + timerValue, Date.now(), timerValue])
      if (Date.now() + timerValue <= Date.now()) {
        clearInterval(intervalRef.current as unknown as number)
        onFinish()
      } else {
        setTimerValue(prev => prev - 1000)
      }
    }, 1000)

    return () => clearInterval(intervalRef.current as unknown as number)
  }, [timerValue, onFinish])

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
    getValues,
    reset,
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
  const [_, _forceUpdate] = useState(Date.now())

  const forceUpdate = () => _forceUpdate(Date.now())

  const isSubmitted = !!submissionError

  const appendInputGroup = useCallback(
    () =>
      append({
        fileKey: '',
        description: '',
      }),
    [append],
  )

  useEffect(() => {
    appendInputGroup()
  }, [appendInputGroup])

  return (
    <Modal
      destroyOnClose
      onCancel={handleCancel}
      onClose={handleCancel}
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
          if (getValues().test.some(x => !x.fileKey)) {
            setSubmissionError('gosh')
            return
          } else {
          }

          try {
            await handleSubmit(handleOk)(values)
          } catch (error) {
            if (!getIsDetailedApiError(error)) throw error

            switch (error.code) {
              default: {
                setSubmissionError(error.code)
              }
            }
          } finally {
            reset()
          }
        }}
      >
        {fields.map((field, index) => (
          <Card
            title={
              <div className="flex justify-between items-center">
                <Typography.Text strong>Вложение {index + 1}</Typography.Text>
                {fields.length < 2 ? null : (
                  <Typography.Link
                    type="danger"
                    onClick={() => remove(index)}
                  >
                    <DeleteOutlined />
                  </Typography.Link>
                )}
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
                    forceUpdate()
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

              {isSubmitted && !getValues().test[index].fileKey ? (
                <Typography.Text type="danger">Необходимо загрузить файл</Typography.Text>
              ) : null}

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

function ModalFinalBoss({ isModalOpen, handleOk, handleCancel, data }: Record<string, any>) {
  const [mark, setMark] = useState(data.mark)

  const contentStyle: React.CSSProperties = {
    margin: 0,
    height: '160px',
    color: '#fff',
    lineHeight: '160px',
    textAlign: 'center',
    background: '#364d79',
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
  }

  return (
    <Modal
      destroyOnClose
      title="Проверка решения"
      open={isModalOpen}
      onOk={() => handleOk({ taskId: data.id, mark })}
      onCancel={handleCancel}
      okText="Оценить"
      cancelText="Закрыть"
      classNames={{
        body: 'my-4',
      }}
    >
      <Carousel
        arrows
        dotPosition="bottom"
        infinite={false}
        className="mb-4"
        prevArrow={<ArrowLeftOutlined className="text-2xl text-blue-500" />}
        nextArrow={<ArrowRightOutlined className="text-2xl text-blue-500" />}
        rootClassName="bg-color-gray"
      >
        {(data.fotos ?? []).map((x: any, i: number) => (
          <div
            key={i}
            style={contentStyle}
          >
            <Image
              src={`/api/File/Download/${x.key}`}
              height={300}
              className="m-auto"
            />
          </div>
        ))}
      </Carousel>
      <div className="flex justify-center">
        <Space>
          <Rate
            onChange={setMark}
            value={mark}
          />
          <Popover
            placement="right"
            title="Алгоритм оценки AI"
            content={
              <Space direction="vertical">
                <Typography.Text>5 - нет ошибок</Typography.Text>
                <Typography.Text>4 - менее 3 ошибок</Typography.Text>
                <Typography.Text>3 - менее 5 ошибок</Typography.Text>
                <Typography.Text>2 - более 5 ошибок</Typography.Text>
              </Space>
            }
          >
            <QuestionCircleOutlined />
          </Popover>
        </Space>
      </div>
    </Modal>
  )
}
